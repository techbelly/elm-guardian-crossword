port module Main exposing (Flags, main)

import Anagram.Dict as AnagramDict
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
                prefill =
                    initialPrefill model

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
                | anagramModal = AnagramOpen { input = prefill, lastSearch = Nothing }
                , dictionary = newDict
              }
            , loadCmd
            )

        CloseAnagramModal ->
            ( { model | anagramModal = AnagramClosed }
            , Cmd.none
            )

        AnagramInputChanged newInput ->
            ( { model
                | anagramModal =
                    case model.anagramModal of
                        AnagramClosed ->
                            AnagramClosed

                        AnagramOpen data ->
                            AnagramOpen { data | input = newInput, lastSearch = Nothing }
              }
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


initialPrefill : ActiveModel -> String
initialPrefill model =
    if not (String.isEmpty (String.trim model.clueSelection)) then
        model.clueSelection

    else
        model.selection
            |> Maybe.andThen (\sel -> Types.lookupClue sel.clueId model.puzzle)
            |> Maybe.map .text
            |> Maybe.withDefault ""


runSearch : ActiveModel -> AnagramModalState
runSearch model =
    case model.anagramModal of
        AnagramClosed ->
            AnagramClosed

        AnagramOpen data ->
            let
                outcome =
                    computeOutcome model.dictionary data.input
            in
            AnagramOpen { data | lastSearch = Just outcome }


computeOutcome : DictionaryState -> String -> AnagramSearchOutcome
computeOutcome dictState input =
    let
        sanitised =
            Search.sanitise input

        len =
            String.length sanitised
    in
    if len < 3 then
        AnagramTooShort

    else if len > 15 then
        AnagramTooLong

    else
        case dictState of
            DictReady dict ->
                case Search.search Search.defaults dict sanitised of
                    [] ->
                        AnagramNoResults

                    results ->
                        AnagramResults results

            _ ->
                AnagramNoResults


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
