port module Main exposing (Flags, main)

import Anagram.Dict as AnagramDict
import Anagram.Enumeration as Enumeration
import Anagram.Fodder as Fodder
import Anagram.Search as Search
import Browser
import Crossword.Decode as Decode
import Crossword.Encode as Encode
import Crossword.Keyboard as Keyboard
import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Selection as Selection
import Crossword.Types as Types
    exposing
        ( ActiveModel
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



-- PORTS


port saveGrid : Json.Encode.Value -> Cmd msg


port scrollIntoView : String -> Cmd msg


port loadDictionary : () -> Cmd msg


port dictionaryLoaded : (Json.Decode.Value -> msg) -> Sub msg


port dictionaryLoadFailed : (String -> msg) -> Sub msg


port clueSelectionChanged : (String -> msg) -> Sub msg


type alias Flags =
    { puzzle : Json.Decode.Value
    , savedGrid : Json.Decode.Value
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
    case Json.Decode.decodeValue Decode.decodePuzzle flags.puzzle of
        Err err ->
            ( Failed (Json.Decode.errorToString err), Cmd.none )

        Ok puzzle ->
            let
                grid =
                    Json.Decode.decodeValue Decode.decodeGrid flags.savedGrid
                        |> Result.withDefault Dict.empty
            in
            ( Active
                { puzzle = puzzle
                , grid = grid
                , selection = Nothing
                , navigationStyle = NYT
                , clueSelection = ""
                , dictionary = DictNotLoaded
                , anagramModal = AnagramClosed
                }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg outerModel =
    case outerModel of
        Failed _ ->
            ( outerModel, Cmd.none )

        Active model ->
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
            ( newModel
            , if needsSave then
                saveGrid (Encode.encodeGrid newModel.grid)

              else
                Cmd.none
            )

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

        NoopClick ->
            ( model, Cmd.none )


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
subscriptions _ =
    Sub.batch
        [ dictionaryLoaded DictionaryLoaded
        , dictionaryLoadFailed DictionaryLoadFailed
        , clueSelectionChanged ClueSelectionChanged
        ]
