port module Main exposing (Flags, main)

import Browser
import Crossword.Decode as Decode
import Crossword.Encode as Encode
import Crossword.Keyboard as Keyboard
import Crossword.Selection as Selection
import Crossword.Types as Types
    exposing
        ( ActiveModel
        , Model(..)
        , Msg(..)
        )
import Crossword.View.Title as ViewTitle
import Crossword.View.Clues as ViewClues
import Crossword.View.Grid as ViewGrid
import Dict
import Html exposing (Html, div, h1, text)
import Html.Attributes as Attr
import Html.Events
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
        , view = view
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
            ( Active { puzzle = puzzle, grid = grid, selection = Nothing }
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
                    Keyboard.handleKey key shiftKey model
            in
            ( newModel
            , if needsSave then
                saveGrid (Encode.encodeGrid newModel.grid)

              else
                Cmd.none
            )

        ClueClicked cid ->
            ( { model | selection = Just (Selection.selectClue cid) }
            , Cmd.none
            )


scrollToClueElement : Types.ClueId -> Cmd Msg
scrollToClueElement cid =
    scrollIntoView (ViewClues.clueElementId cid)


view : Model -> Html Msg
view outerModel =
    case outerModel of
        Failed err ->
            div []
                [ h1 [] [ text "Error loading crossword" ]
                , Html.pre [] [ text err ]
                ]

        Active model ->
            div
                [ Attr.class "crossword"
                , Attr.tabindex 0
                , Html.Events.preventDefaultOn "keydown"
                    (Json.Decode.map2
                        (\key shift ->
                            ( KeyPressed key shift
                            , Keyboard.shouldPreventDefault key
                            )
                        )
                        (Json.Decode.field "key" Json.Decode.string)
                        (Json.Decode.field "shiftKey" Json.Decode.bool)
                    )
                ]
                [ ViewTitle.viewTitle model.puzzle
                , ViewClues.viewStickyBar model.puzzle model.selection
                , div [ Attr.class "crossword__content" ]
                    [ ViewGrid.viewGrid model.puzzle model.grid model.selection
                    , ViewClues.viewCluePanel model.puzzle model.grid model.selection
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
