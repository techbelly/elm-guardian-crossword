port module Main exposing (main)

import Browser
import Crossword.Decode as Decode
import Crossword.Encode as Encode
import Crossword.Grid as Grid
import Crossword.Navigation as Navigation
import Crossword.Selection as Selection
import Crossword.Types as Types
    exposing
        ( ActiveModel
        , Arrow(..)
        , CellValue(..)
        , Model(..)
        , Msg(..)
        , Puzzle
        , Selection
        )
import Crossword.View.Clues as ViewClues
import Crossword.View.Grid as ViewGrid
import Dict
import Html exposing (Html, div, h1, span, text)
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
                    case Json.Decode.decodeValue Decode.decodeGrid flags.savedGrid of
                        Ok savedGrid ->
                            savedGrid

                        Err _ ->
                            Dict.empty
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
            handleKey key shiftKey model

        ClueClicked eid ->
            ( { model | selection = Just { clueId = eid, cellIndex = 0 } }
            , Cmd.none
            )



-- Key categorization


type KeyAction
    = Letter Char
    | Backspace
    | Arrow Arrow
    | Tab
    | ShiftTab
    | Unhandled


categorizeKey : String -> Bool -> KeyAction
categorizeKey key shiftKey =
    case key of
        "Backspace" ->
            Backspace

        "Delete" ->
            Backspace

        "ArrowLeft" ->
            Arrow ArrowLeft

        "ArrowRight" ->
            Arrow ArrowRight

        "ArrowUp" ->
            Arrow ArrowUp

        "ArrowDown" ->
            Arrow ArrowDown

        "Tab" ->
            if shiftKey then
                ShiftTab

            else
                Tab

        _ ->
            case String.uncons key of
                Just ( ch, "" ) ->
                    if isValidChar ch then
                        Letter (Char.toUpper ch)

                    else
                        Unhandled

                _ ->
                    Unhandled


isValidChar : Char -> Bool
isValidChar ch =
    let
        code =
            Char.toCode ch
    in
    (code >= 0x41 && code <= 0x5A)
        || (code >= 0x61 && code <= 0x7A)
        || (code >= 0x30 && code <= 0x39)
        || (code >= 0xC0 && code <= 0xFF)


handleKey : String -> Bool -> ActiveModel -> ( ActiveModel, Cmd Msg )
handleKey key shiftKey model =
    case model.selection of
        Nothing ->
            ( model, Cmd.none )

        Just sel ->
            let
                puzzle =
                    model.puzzle
            in
            case categorizeKey key shiftKey of
                Letter ch ->
                    let
                        pos =
                            selectionPosition puzzle sel

                        newGrid =
                            Grid.set pos (Filled ch) model.grid

                        newSel =
                            Navigation.nextCell puzzle sel
                    in
                    ( { model | grid = newGrid, selection = Just newSel }
                    , saveGrid (Encode.encodeGrid newGrid)
                    )

                Backspace ->
                    let
                        pos =
                            selectionPosition puzzle sel

                        currentValue =
                            Grid.get pos model.grid
                    in
                    case currentValue of
                        Filled _ ->
                            let
                                newGrid =
                                    Grid.set pos Empty model.grid
                            in
                            ( { model | grid = newGrid }
                            , saveGrid (Encode.encodeGrid newGrid)
                            )

                        Empty ->
                            ( { model | selection = Just (Navigation.prevCell puzzle sel) }
                            , Cmd.none
                            )

                Arrow dir ->
                    ( { model | selection = Just (Navigation.arrowMove dir puzzle sel) }
                    , Cmd.none
                    )

                Tab ->
                    ( { model | selection = Just (Navigation.nextClue puzzle sel) }
                    , Cmd.none
                    )

                ShiftTab ->
                    ( { model | selection = Just (Navigation.prevClue puzzle sel) }
                    , Cmd.none
                    )

                Unhandled ->
                    ( model, Cmd.none )


selectionPosition : Puzzle -> Selection -> Types.Position
selectionPosition puzzle sel =
    case lookupClue sel.clueId puzzle of
        Just entry ->
            Grid.clueCell sel.cellIndex entry

        Nothing ->
            ( 0, 0 )


lookupClue : Types.ClueId -> Puzzle -> Maybe Types.Clue
lookupClue cid puzzle =
    puzzle.clues
        |> List.filter (\c -> c.id == cid)
        |> List.head


scrollToClueElement : Types.ClueId -> Cmd Msg
scrollToClueElement cid =
    let
        dir =
            case cid.direction of
                Types.Across ->
                    "across"

                Types.Down ->
                    "down"
    in
    scrollIntoView ("clue-" ++ String.fromInt cid.number ++ "-" ++ dir)


shouldPreventDefault : String -> Bool
shouldPreventDefault key =
    List.member key
        [ "Tab"
        , "ArrowLeft"
        , "ArrowRight"
        , "ArrowUp"
        , "ArrowDown"
        , "Backspace"
        ]


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
                            , shouldPreventDefault key
                            )
                        )
                        (Json.Decode.field "key" Json.Decode.string)
                        (Json.Decode.field "shiftKey" Json.Decode.bool)
                    )
                ]
                [ h1 [ Attr.class "crossword__title" ]
                    (text model.puzzle.name
                        :: (case model.puzzle.setter of
                                Just setter ->
                                    [ span
                                        [ Attr.style "font-weight" "normal"
                                        , Attr.style "font-style" "italic"
                                        ]
                                        [ text (" by " ++ setter) ]
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                , ViewClues.viewStickyBar model.puzzle model.selection
                , div [ Attr.class "crossword__content" ]
                    [ ViewGrid.viewGrid model.puzzle model.grid model.selection
                    , ViewClues.viewCluePanel model.puzzle model.grid model.selection
                    ]
                ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
