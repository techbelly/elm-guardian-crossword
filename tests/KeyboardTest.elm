module KeyboardTest exposing (suite)

import Crossword.Keyboard as Keyboard
import Crossword.Navigation.Guardian as Guardian
import Expect
import Fixture
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Keyboard"
        [ shouldPreventDefaultSuite
        , handleKeySuite
        ]


shouldPreventDefaultSuite : Test
shouldPreventDefaultSuite =
    describe "shouldPreventDefault"
        [ test "returns True for navigation keys" <|
            \_ ->
                List.map Keyboard.shouldPreventDefault
                    [ "Tab", "ArrowLeft", "ArrowRight", "ArrowUp", "ArrowDown", "Backspace", "Delete" ]
                    |> Expect.equal (List.repeat 7 True)
        , test "returns False for letter keys" <|
            \_ ->
                Keyboard.shouldPreventDefault "a" |> Expect.equal False
        , test "returns False for unhandled keys like Enter" <|
            \_ ->
                Keyboard.shouldPreventDefault "Enter" |> Expect.equal False
        ]


handleKeySuite : Test
handleKeySuite =
    let
        runKey key shiftKey rows =
            Fixture.fromGrid rows
                |> Fixture.toModel
                |> Keyboard.handleKey Guardian.strategy key shiftKey
                |> Tuple.first
                |> Fixture.fromModel
                |> Fixture.renderFixture
    in
    describe "handleKey"
        [ test "Delete clears a filled cell like Backspace" <|
            \_ ->
                runKey "Delete" False
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    |> Expect.equal
                        [ "→_  B  C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        , test "extended character é is accepted and upcased" <|
            \_ ->
                runKey "é" False
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    |> Expect.equal
                        [ " É →B  C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        , test "Enter is ignored" <|
            \_ ->
                runKey "Enter" False
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    |> Expect.equal
                        [ "→A  B  C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        , test "Tab with Shift held behaves like ShiftTab" <|
            \_ ->
                -- ShiftTab from 1-across wraps to the last clue (2-down) in Guardian order
                runKey "Tab" True
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    |> Expect.equal
                        [ " A  B ↓C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        , test "has no effect when there is no selection" <|
            \_ ->
                runKey "a" False
                    [ " A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    |> Expect.equal
                        [ " A  B  C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        ]
