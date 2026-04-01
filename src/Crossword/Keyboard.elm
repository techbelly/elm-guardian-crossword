module Crossword.Keyboard exposing (handleArrow, handleBackspace, handleKey, handleLetter, handleShiftTab, handleTab, shouldPreventDefault)

import Crossword.Grid as Grid
import Crossword.Selection as Selection
import Crossword.Types
    exposing
        ( ActiveModel
        , Arrow(..)
        , CellValue(..)
        , NavigationStrategy
        )


type KeyAction
    = Letter Char
    | Backspace
    | Arrow Arrow
    | Tab
    | ShiftTab
    | Unhandled


handleKey : NavigationStrategy -> String -> Bool -> ActiveModel -> ( ActiveModel, Bool )
handleKey strategy key shiftKey model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just _ ->
            case categorizeKey key shiftKey of
                Letter ch ->
                    handleLetter strategy ch model

                Backspace ->
                    handleBackspace model

                Arrow dir ->
                    handleArrow dir model

                Tab ->
                    handleTab strategy model

                ShiftTab ->
                    handleShiftTab strategy model

                Unhandled ->
                    ( model, False )


handleLetter : NavigationStrategy -> Char -> ActiveModel -> ( ActiveModel, Bool )
handleLetter strategy ch model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just sel ->
            case Selection.selectionPosition sel model.puzzle of
                Nothing ->
                    ( model, False )

                Just pos ->
                    let
                        wasBlank =
                            Grid.get pos model.grid == Empty

                        newGrid =
                            Grid.set pos (Filled ch) model.grid

                        newSel =
                            strategy.afterLetter wasBlank newGrid model.puzzle sel
                    in
                    ( { model | grid = newGrid, selection = Just newSel }
                    , True
                    )


handleBackspace : ActiveModel -> ( ActiveModel, Bool )
handleBackspace model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just sel ->
            case Selection.selectionPosition sel model.puzzle of
                Nothing ->
                    ( model, False )

                Just pos ->
                    case Grid.get pos model.grid of
                        Filled _ ->
                            ( { model | grid = Grid.set pos Empty model.grid }
                            , True
                            )

                        Empty ->
                            let
                                isFirst =
                                    Selection.groupSelections sel.clueId model.puzzle
                                        |> List.head
                                        |> Maybe.map (\first -> first == sel)
                                        |> Maybe.withDefault True
                            in
                            if isFirst then
                                ( model, False )

                            else
                                ( { model | selection = Just (Selection.prevCell model.puzzle sel) }
                                , False
                                )


handleArrow : Arrow -> ActiveModel -> ( ActiveModel, Bool )
handleArrow dir model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just sel ->
            ( { model | selection = Just (Selection.arrowMove dir model.puzzle sel) }
            , False
            )


handleTab : NavigationStrategy -> ActiveModel -> ( ActiveModel, Bool )
handleTab strategy model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just sel ->
            ( { model | selection = Just (strategy.nextClue model.grid model.puzzle sel) }
            , False
            )


handleShiftTab : NavigationStrategy -> ActiveModel -> ( ActiveModel, Bool )
handleShiftTab strategy model =
    case model.selection of
        Nothing ->
            ( model, False )

        Just sel ->
            ( { model | selection = Just (strategy.prevClue model.grid model.puzzle sel) }
            , False
            )


shouldPreventDefault : String -> Bool
shouldPreventDefault key =
    case categorizeKey key False of
        Unhandled ->
            False

        Letter _ ->
            False

        _ ->
            True


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
    Char.isAlphaNum ch || (Char.toCode ch >= 0xC0 && Char.toCode ch <= 0xFF)
