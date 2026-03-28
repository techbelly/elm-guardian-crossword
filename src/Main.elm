port module Main exposing (Flags, main)

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


port saveGrid : Json.Encode.Value -> Cmd msg


port scrollIntoView : String -> Cmd msg


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
    Sub.none
