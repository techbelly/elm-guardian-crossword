port module Main exposing (Flags, main)

import Anagram.Dict as AnagramDict
import Anagram.Enumeration as Enumeration
import Anagram.Fodder as Fodder
import Anagram.Search as Search
import Browser
import Browser.Events
import Crossword.Decode as Decode
import Crossword.Encode as Encode
import Crossword.History as History
import Crossword.Keyboard as Keyboard
import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Selection as Selection
import Crossword.Timer as Timer
import Crossword.Types as Types
    exposing
        ( ActiveModel
        , LandingModel
        , LoadState(..)
        , AnagramModalData
        , AnagramModalState(..)
        , AnagramSearchOutcome(..)
        , DictionaryState(..)
        , Model(..)
        , Msg(..)
        , NavigationStrategy
        , NavigationStyle(..)
        )
import Crossword.View.App as ViewApp
import Crossword.View.Clues as ViewClues
import Dict
import Json.Decode
import Json.Encode
import Process
import Task
import Time



-- PORTS


port saveGrid : Json.Encode.Value -> Cmd msg


port saveHistoryEntry : Json.Encode.Value -> Cmd msg


port loadPuzzle : String -> Cmd msg


port puzzleLoaded : (Json.Decode.Value -> msg) -> Sub msg


port puzzleLoadFailed : (String -> msg) -> Sub msg


port scrollIntoView : String -> Cmd msg


port loadDictionary : () -> Cmd msg


port dictionaryLoaded : (Json.Decode.Value -> msg) -> Sub msg


port dictionaryLoadFailed : (String -> msg) -> Sub msg


port clueSelectionChanged : (String -> msg) -> Sub msg


type alias Flags =
    { history : Json.Decode.Value
    , now : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = ViewApp.view
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Landing
        { path = ""
        , history =
            Json.Decode.decodeValue History.decoder flags.history
                |> Result.withDefault []
        , load = NotLoading
        , now = Time.millisToPosix flags.now
        }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg outerModel =
    case outerModel of
        Landing model ->
            updateLanding msg model

        Active model ->
            case msg of
                BackToLanding ->
                    ( Landing
                        { path = ""
                        , history = History.merge (historyEntry model) model.history
                        , load = NotLoading
                        , now = model.now
                        }
                    , Cmd.none
                    )

                _ ->
                    let
                        ( newModel, cmd ) =
                            updateActive msg model

                        oldClueId =
                            Maybe.map .clueId model.selection

                        newClueId =
                            Maybe.map .clueId newModel.selection

                        scrollCmd =
                            if newClueId /= oldClueId then
                                newClueId
                                    |> Maybe.map scrollToClueElement
                                    |> Maybe.withDefault Cmd.none

                            else
                                Cmd.none
                    in
                    ( Active newModel, Cmd.batch [ cmd, scrollCmd ] )


updateLanding : Msg -> LandingModel -> ( Model, Cmd Msg )
updateLanding msg model =
    case msg of
        PathChanged path ->
            ( Landing { model | path = path }, Cmd.none )

        LoadRequested ->
            ( Landing { model | load = Loading }
            , loadPuzzle (String.trim model.path)
            )

        HistoryEntryClicked path ->
            ( Landing { model | path = path, load = Loading }
            , loadPuzzle path
            )

        PuzzleLoaded value ->
            ( startSolving model value, Cmd.none )

        PuzzleLoadFailed err ->
            ( Landing { model | load = LoadFailed err }, Cmd.none )

        Tick now ->
            ( Landing { model | now = now }, Cmd.none )

        _ ->
            ( Landing model, Cmd.none )


{-| Hand a freshly fetched puzzle over to the solving view, restoring whatever
grid and elapsed time an earlier visit left behind.
-}
startSolving : LandingModel -> Json.Decode.Value -> Model
startSolving model value =
    case Json.Decode.decodeValue (Json.Decode.field "puzzle" Decode.decodePuzzle) value of
        Err err ->
            Landing { model | load = LoadFailed (Json.Decode.errorToString err) }

        Ok puzzle ->
            let
                path =
                    pathOf puzzle

                previous =
                    model.history |> List.filter (\entry -> entry.path == path) |> List.head
            in
            Active
                { puzzle = puzzle
                , path = path
                , grid =
                    Json.Decode.decodeValue (Json.Decode.field "savedGrid" Decode.decodeGrid) value
                        |> Result.withDefault Dict.empty
                , selection = Nothing
                , navigationStyle = NYT
                , clueSelection = ""
                , dictionary = DictNotLoaded
                , anagramModal = AnagramClosed
                , timer = Timer.resume (previous |> Maybe.map .elapsed |> Maybe.withDefault 0)
                , now = model.now
                , history = model.history
                }


updateActive : Msg -> ActiveModel -> ( ActiveModel, Cmd Msg )
updateActive msg model =
    case msg of
        CellClicked pos ->
            ( { model | selection = Selection.selectCell pos model.selection model.puzzle }
            , Cmd.none
            )

        KeyPressed key shiftKey ->
            let
                ( newModel, needsSave ) =
                    Keyboard.handleKey (strategyFor model.navigationStyle) key shiftKey model
            in
            if needsSave then
                let
                    solving =
                        { newModel | timer = Timer.touch model.now newModel.timer }
                in
                ( solving
                , Cmd.batch
                    [ saveGrid (Encode.encodeGrid solving.puzzle.id solving.grid)
                    , saveHistoryEntry (History.encode (historyEntry solving))
                    ]
                )

            else
                ( newModel, Cmd.none )

        ClueClicked cid ->
            ( { model | selection = Just ((strategyFor model.navigationStyle).selectClue model.grid model.puzzle cid) }
            , Cmd.none
            )

        SetNavigation style ->
            ( { model | navigationStyle = style }
            , Cmd.none
            )

        ClueSelectionChanged text ->
            ( { model | clueSelection = text }
            , Cmd.none
            )

        OpenAnagramModal ->
            let
                ( newDict, loadCmd ) =
                    case model.dictionary of
                        DictNotLoaded ->
                            ( DictLoading, loadDictionary () )

                        DictFailed _ ->
                            ( DictLoading, loadDictionary () )

                        _ ->
                            ( model.dictionary, Cmd.none )
            in
            ( { model
                | anagramModal = AnagramOpen (initialModalData model)
                , dictionary = newDict
              }
            , loadCmd
            )

        CloseAnagramModal ->
            ( { model | anagramModal = AnagramClosed }
            , Cmd.none
            )

        AnagramTokenToggled index ->
            ( mapModal (\data -> { data | tokens = Fodder.toggle index data.tokens }) model
            , Cmd.none
            )

        AnagramExtraChanged extra ->
            ( mapModal (\data -> { data | extra = extra }) model
            , Cmd.none
            )

        AnagramEnumerationChanged enumeration ->
            ( mapModal (\data -> { data | enumeration = enumeration }) model
            , Cmd.none
            )

        AnagramSubmit ->
            case ( model.dictionary, model.anagramModal ) of
                ( DictReady _, AnagramOpen data ) ->
                    ( { model
                        | anagramModal =
                            AnagramOpen { data | lastSearch = Just AnagramSearching }
                      }
                    , Process.sleep 0 |> Task.perform (\_ -> AnagramRunSearch)
                    )

                _ ->
                    ( model, Cmd.none )

        AnagramRunSearch ->
            ( { model | anagramModal = runSearch model }
            , Cmd.none
            )

        DictionaryLoaded value ->
            case Json.Decode.decodeValue AnagramDict.decoder value of
                Ok dict ->
                    ( { model | dictionary = DictReady dict }
                    , Cmd.none
                    )

                Err err ->
                    ( { model | dictionary = DictFailed (Json.Decode.errorToString err) }
                    , Cmd.none
                    )

        DictionaryLoadFailed err ->
            ( { model | dictionary = DictFailed err }
            , Cmd.none
            )

        Tick now ->
            let
                ticked =
                    { model | now = now, timer = Timer.tick now model.timer }
            in
            ( ticked
            , if Timer.isRunning ticked.timer && banking now then
                saveHistoryEntry (History.encode (historyEntry ticked))

              else
                Cmd.none
            )

        VisibilityChanged visibility ->
            case visibility of
                Browser.Events.Hidden ->
                    ( { model | timer = Timer.pause model.timer }
                    , saveHistoryEntry (History.encode (historyEntry model))
                    )

                Browser.Events.Visible ->
                    ( model, Cmd.none )

        PathChanged _ ->
            ( model, Cmd.none )

        LoadRequested ->
            ( model, Cmd.none )

        HistoryEntryClicked _ ->
            ( model, Cmd.none )

        PuzzleLoaded _ ->
            ( model, Cmd.none )

        PuzzleLoadFailed _ ->
            ( model, Cmd.none )

        BackToLanding ->
            ( model, Cmd.none )

        NoopClick ->
            ( model, Cmd.none )


{-| Elapsed time is written back every fifteen seconds rather than every tick;
losing at most that much to a closed tab is a fair trade for not writing to
localStorage once a second.
-}
banking : Time.Posix -> Bool
banking now =
    remainderBy 15 (Time.posixToMillis now // 1000) == 0


{-| Snapshot the selected clue into modal state. Tokens come from the clue
selection when there is one, so a highlighted phrase narrows what's on offer,
while the enumeration is always read from the whole clue.
-}
initialModalData : ActiveModel -> AnagramModalData
initialModalData model =
    let
        clueText =
            selectedClue model |> Maybe.map .text |> Maybe.withDefault ""

        tokenSource =
            if String.isEmpty (String.trim model.clueSelection) then
                clueText

            else
                model.clueSelection
    in
    { tokens = Fodder.tokenise tokenSource
    , extra = ""
    , enumeration =
        Enumeration.fromClue clueText
            |> Maybe.map Enumeration.toText
            |> Maybe.withDefault ""
    , lastSearch = Nothing
    }


selectedClue : ActiveModel -> Maybe Types.Clue
selectedClue model =
    model.selection
        |> Maybe.andThen (\sel -> Types.lookupClue sel.clueId model.puzzle)


{-| Apply a change to the open modal, discarding any results it was showing —
every field here changes what a search would return.
-}
mapModal : (AnagramModalData -> AnagramModalData) -> ActiveModel -> ActiveModel
mapModal f model =
    case model.anagramModal of
        AnagramClosed ->
            model

        AnagramOpen data ->
            { model | anagramModal = AnagramOpen (f { data | lastSearch = Nothing }) }


runSearch : ActiveModel -> AnagramModalState
runSearch model =
    case model.anagramModal of
        AnagramClosed ->
            AnagramClosed

        AnagramOpen data ->
            AnagramOpen { data | lastSearch = Just (computeOutcome model.dictionary data) }


computeOutcome : DictionaryState -> AnagramModalData -> AnagramSearchOutcome
computeOutcome dictState data =
    let
        input =
            Fodder.letters data.extra data.tokens
    in
    if String.length input < 3 then
        AnagramTooShort

    else if String.length input > 15 then
        AnagramTooLong

    else
        case dictState of
            DictReady dict ->
                let
                    defaults =
                        Search.defaults

                    lengths =
                        Enumeration.parse data.enumeration
                            |> Maybe.map (Search.OneOf << Enumeration.alternatives)
                            |> Maybe.withDefault Search.AnyLengths
                in
                case Search.search { defaults | lengths = lengths } dict input of
                    [] ->
                        emptyOutcome lengths

                    results ->
                        AnagramResults results

            _ ->
                AnagramNoResults


{-| Distinguish "nothing anagrams these letters" from "nothing anagrams them
into those lengths", which is recoverable by clearing the lengths field.
-}
emptyOutcome : Search.WordLengths -> AnagramSearchOutcome
emptyOutcome lengths =
    case lengths of
        Search.AnyLengths ->
            AnagramNoResults

        Search.OneOf _ ->
            AnagramNoResultsForLengths


{-| The puzzle as it stands, in the shape the history list wants. The path is
what the Guardian fetch needs, which is the puzzle id without its leading
"crosswords/".
-}
historyEntry : ActiveModel -> History.Entry
historyEntry model =
    { path = model.path
    , series = model.puzzle.crosswordType
    , number = model.puzzle.puzzleNumber
    , name = model.puzzle.name
    , setter = model.puzzle.setter
    , published = model.puzzle.published
    , lastOpened = model.now
    , elapsed = Timer.elapsed model.timer
    , filled = filledCells model.grid
    , cells = Dict.size model.puzzle.cellInfos
    }


pathOf : Types.Puzzle -> String
pathOf puzzle =
    -- Guardian ids read "crosswords/cryptic/29963"; the fetch wants the rest.
    case String.split "/" puzzle.id of
        "crosswords" :: rest ->
            String.join "/" rest

        _ ->
            puzzle.id


filledCells : Types.Grid -> Int
filledCells grid =
    grid
        |> Dict.values
        |> List.filter (\value -> value /= Types.Empty)
        |> List.length


strategyFor : NavigationStyle -> NavigationStrategy
strategyFor style =
    case style of
        Guardian ->
            Guardian.strategy

        NYT ->
            NYT.strategy


scrollToClueElement : Types.ClueId -> Cmd Msg
scrollToClueElement cid =
    scrollIntoView (ViewClues.clueElementId cid)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dictionaryLoaded DictionaryLoaded
        , dictionaryLoadFailed DictionaryLoadFailed
        , clueSelectionChanged ClueSelectionChanged
        , puzzleLoaded PuzzleLoaded
        , puzzleLoadFailed PuzzleLoadFailed

        -- The clock ticks whether or not the timer is running: it is also how
        -- the model learns what time it is, which typing needs to know.
        , Time.every 1000 Tick
        , case model of
            Active _ ->
                Browser.Events.onVisibilityChange VisibilityChanged

            Landing _ ->
                Sub.none
        ]
