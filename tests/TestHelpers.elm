module TestHelpers exposing
    ( uitest
    , uitestFixture
    , UIAction(..)
    )

import Crossword.Keyboard as Keyboard
import Crossword.Selection as Selection
import Crossword.Types
    exposing
        ( Arrow
        , ClueId
        , NavigationStrategy
        )
import Expect
import Fixture

type UIAction
     = Type Char
     | Backspace
     | Tab
     | ShiftTab
     | Arrow Arrow
     | Click ( Int, Int )
     | Select ClueId

doAction :
    UIAction
    -> NavigationStrategy
    -> Fixture.Fixture
    -> Fixture.Fixture
doAction action strategy fixture =
    case action of
        Type ch ->
            Keyboard.handleLetter strategy ch (Fixture.toModel fixture)
                |> Tuple.first
                |> Fixture.fromModel
        Backspace ->
            Keyboard.handleBackspace (Fixture.toModel fixture)
                |> Tuple.first
                |> Fixture.fromModel

        Tab ->
            Keyboard.handleTab strategy (Fixture.toModel fixture)
                |> Tuple.first
                |> Fixture.fromModel

        ShiftTab ->
            Keyboard.handleShiftTab strategy (Fixture.toModel fixture)
                |> Tuple.first
                |> Fixture.fromModel

        Arrow dir ->
            Keyboard.handleArrow dir (Fixture.toModel fixture)
                |> Tuple.first
                |> Fixture.fromModel

        Click pos ->
            { fixture
                | selection =
                    Selection.selectCell pos fixture.selection fixture.puzzle
            }

        Select cid ->
            { fixture
                | selection =
                    Just (strategy.selectClue fixture.grid fixture.puzzle cid)
            }

uitest :
    NavigationStrategy
    -> List String
    -> UIAction
    -> List String
    -> Expect.Expectation
uitest strategy inputRows uiAction expectedRows =
    uitestFixture strategy (Fixture.fromGrid inputRows) uiAction expectedRows


uitestFixture :
    NavigationStrategy
    -> Fixture.Fixture
    -> UIAction
    -> List String
    -> Expect.Expectation
uitestFixture strategy fixture uiAction expectedRows =
    fixture
        |> doAction uiAction strategy
        |> Fixture.renderFixture
        |> Expect.equal expectedRows

