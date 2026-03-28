module Crossword.Keyboard exposing (handleKey, shouldPreventDefault)

import Crossword.Grid as Grid
import Crossword.Selection as Selection
import Crossword.Types
    exposing
        ( ActiveModel
        , Arrow(..)
        , CellValue(..)
        )


type KeyAction
    = Letter Char
    | Backspace
    | Arrow Arrow
    | Tab
    | ShiftTab
    | Unhandled


handleKey : String -> Bool -> ActiveModel -> ( ActiveModel, Bool )
handleKey key shiftKey model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just sel ->
            let
                puzzle =
                    model.puzzle
            in
            case categorizeKey key shiftKey of
                Letter ch ->
                    case Selection.selectionPosition sel puzzle of
                        Nothing ->
                            ( model, False )

                        Just pos ->
                            let
                                newGrid =
                                    Grid.set pos (Filled ch) model.grid
                            in
                            ( { model | grid = newGrid, selection = Just (Selection.nextCell puzzle sel) }
                            , True
                            )

                Backspace ->
                    case Selection.selectionPosition sel puzzle of
                        Nothing ->
                            ( model, False )

                        Just pos ->
                            case Grid.get pos model.grid of
                                Filled _ ->
                                    let
                                        newGrid =
                                            Grid.set pos Empty model.grid
                                    in
                                    ( { model | grid = newGrid }
                                    , True
                                    )

                                Empty ->
                                    ( { model | selection = Just (Selection.prevCell puzzle sel) }
                                    , False
                                    )

                Arrow dir ->
                    ( { model | selection = Just (Selection.arrowMove dir puzzle sel) }
                    , False
                    )

                Tab ->
                    ( { model | selection = Just (Selection.nextClue puzzle sel) }
                    , False
                    )

                ShiftTab ->
                    ( { model | selection = Just (Selection.prevClue puzzle sel) }
                    , False
                    )

                Unhandled ->
                    ( model, False )


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
