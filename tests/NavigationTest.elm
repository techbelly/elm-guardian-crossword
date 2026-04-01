module NavigationTest exposing (suite)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Types exposing (Arrow(..), Direction(..))
import Fixture
import Test exposing (Test, describe, test)
import TestHelpers exposing (uitest, uitestFixture, UIAction(..))


{- The shared 3×3 grid structure used across all tests:

       col: 0  1  2
    row 0:  .  .  .    (row 0 is part of 1-across, 1-down, 2-down)
    row 1:  .  ■  .    (black at centre; row 1 white cells are (0,1) and (2,1) only)
    row 2:  .  .  .    (row 2 is 3-across, and the last cells of 1-down, 2-down)

    Clue numbers assigned in reading order:
      1 → (0,0): starts 1-across (length 3) and 1-down (length 3)
      2 → (2,0): starts 2-down  (length 3)
      3 → (0,2): starts 3-across (length 3)

    Guardian clue order: [1-across, 3-across, 1-down, 2-down]
-}


suite : Test
suite =
    describe "Navigation"
        [ guardianSuite
        , nytSuite
        , arrowSuite
        , clickSuite
        , groupSuite
        ]


guardianSuite : Test
guardianSuite =
    describe "Guardian"
        [ test "nextClue advances to the next clue" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Tab
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "nextClue from the last clue wraps to the first" <|
            \_ ->
                -- 2-down is last in Guardian order; wraps back to 1-across
                uitest Guardian.strategy
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Tab
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "prevClue goes to the previous clue" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
                    ShiftTab
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "prevClue from the first clue wraps to the last" <|
            \_ ->
                -- 2-down is last in Guardian order
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    ShiftTab
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter advances one cell within the clue" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'A')
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter at the last cell of a clue wraps to its first" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'C')
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "selectClue always selects the first cell, ignoring blanks" <|
            \_ ->
                -- Guardian ignores blanks; always goes to cell index 0
                uitest Guardian.strategy
                    [ " A  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Select { number = 1, direction = Across })
                    [ "→A  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        ]


nytSuite : Test
nytSuite =
    describe "NYT"
        [ test "nextClue skips filled clues and lands on the first blank of the next unfilled clue" <|
            \_ ->
                -- 1-across is full; 3-across has a blank at (0,2)
                uitest NYT.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
                    Tab
                    [ " A  B  C "
                    , " D  *  E "
                    , "→_  G  H "
                    ]
        , test "nextClue stays put when every clue is filled" <|
            \_ ->
                uitest NYT.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Tab
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "prevClue goes to the first blank of the previous unfilled clue" <|
            \_ ->
                -- Starting at 2-down; going backward, 1-down has a blank at (0,2)
                uitest NYT.strategy
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
                    ShiftTab
                    [ " A  B  C "
                    , " D  *  E "
                    , "↓_  G  H "
                    ]
        , test "afterLetter in a blank advances to the next blank in the clue" <|
            \_ ->
                -- typing A in the blank; next blank in 1-across is the _ at (2,0)
                uitest NYT.strategy
                    [ "→_  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'A')
                    [ " A  B →_ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter at the last blank moves to the first blank of the next unfilled clue" <|
            \_ ->
                -- typing A fills the last blank in 1-across; next unfilled is 3-across
                uitest NYT.strategy
                    [ "→_  B  C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
                    (Type 'A')
                    [ " A  B  C "
                    , " D  *  E "
                    , "→_  G  H "
                    ]
        , test "afterLetter on a filled cell advances to the next cell" <|
            \_ ->
                uitest NYT.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'B')
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter at the last filled cell stays put" <|
            \_ ->
                uitest NYT.strategy
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'C')
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "selectClue with blanks selects the first blank, not the first cell" <|
            \_ ->
                uitest NYT.strategy
                    [ " A  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Select { number = 1, direction = Across })
                    [ " A  B →_ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "selectClue on a fully filled clue selects its first cell" <|
            \_ ->
                uitest NYT.strategy
                    [ " A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Select { number = 1, direction = Across })
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        ]


arrowSuite : Test
arrowSuite =
    describe "Arrow keys"
        [ test "ArrowRight moves to the next white cell in the row" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowRight)
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowRight skips over black cells" <|
            \_ ->
                -- Row 1 white cells are [(0,1), (2,1)]; ArrowRight from D jumps to E
                uitest Guardian.strategy
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowRight)
                    [ " A  B  C "
                    , " D  * ↓E "
                    , " F  G  H "
                    ]
        , test "ArrowRight wraps from the last white cell in a row to the first" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowRight)
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowLeft moves to the previous white cell in the row" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowLeft)
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowDown moves to the next white cell in the column, preferring Down" <|
            \_ ->
                -- Col 0: [(0,0),(0,1),(0,2)]; ArrowDown from A lands on D in 1-down
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowDown)
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowUp moves to the previous white cell in the column, preferring Down" <|
            \_ ->
                -- Col 0: [(0,0),(0,1),(0,2)]; ArrowUp from D lands on A
                uitest Guardian.strategy
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowUp)
                    [ "↓A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowDown skips over black cells in a column" <|
            \_ ->
                -- Col 1: [(1,0),(1,2)]; ArrowDown from B skips black at (1,1) and lands on G
                uitest Guardian.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Arrow ArrowDown)
                    [ " A  B  C "
                    , " D  *  E "
                    , " F →G  H "
                    ]
        ]


clickSuite : Test
clickSuite =
    describe "Cell clicks"
        [ test "clicking a new cell moves the selection there" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 0, 2 ))
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "clicking a cell that only belongs to a down clue selects it in down" <|
            \_ ->
                -- D at (0,1) is DownOnly; freshSelection falls back from Across to Down
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 0, 1 ))
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
        , test "clicking the selected cell again toggles the active direction" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 0, 0 ))
                    [ "↓A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "clicking a black cell does not change the selection" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 1, 1 ))
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "first click with no prior selection creates a fresh selection" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 0, 2 ))
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "clicking the selected cell again when it has only one direction does not toggle" <|
            \_ ->
                -- D at (0,1) is DownOnly; there is no Across clue to toggle to
                uitest Guardian.strategy
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 0, 1 ))
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
        , test "first click on a cell that only starts a down clue selects it in down" <|
            \_ ->
                -- C at (2,0) starts 2-down but not an across clue; freshSelection picks Down
                uitest Guardian.strategy
                    [ " A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 2, 0 ))
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "clicking a different cell within the same down clue moves the selection there" <|
            \_ ->
                -- A at (0,0) selected in 1-down; clicking F at (0,2) stays in 1-down
                uitest Guardian.strategy
                    [ "↓A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Click ( 0, 2 ))
                    [ " A  B  C "
                    , " D  *  E "
                    , "↓F  G  H "
                    ]
        ]


groupSuite : Test
groupSuite =
    describe "Grouped (linked) clues"
        [ test "Guardian: typing at the last cell of the first linked clue continues into the second" <|
            \_ ->
                uitestFixture Guardian.strategy
                    (Fixture.fromGrid
                        [ " A  B →C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
                        |> Fixture.withGroup "ABC,FGH"
                    )
                    (Type 'C')
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "Guardian: typing at the last cell of the last linked clue wraps to the first" <|
            \_ ->
                uitestFixture Guardian.strategy
                    (Fixture.fromGrid
                        [ " A  B  C "
                        , " D  *  E "
                        , " F  G →H "
                        ]
                        |> Fixture.withGroup "ABC,FGH"
                    )
                    (Type 'H')
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "Guardian: backspace at the first cell of the second linked clue goes to the last of the first" <|
            \_ ->
                uitestFixture Guardian.strategy
                    (Fixture.fromGrid
                        [ " A  B  C "
                        , " D  *  E "
                        , "→_  G  H "
                        ]
                        |> Fixture.withGroup "ABC,_GH"
                    )
                    Backspace
                    [ " A  B →C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
        , test "NYT: typing in a blank searches for the next blank across the whole linked group" <|
            \_ ->
                -- 1-across is fully filled; 3-across has a blank at F(0,2)
                -- Without grouping, afterLetter True would jump to a different clue.
                -- With grouping, it finds F as the next blank within the group.
                uitestFixture NYT.strategy
                    (Fixture.fromGrid
                        [ " A  B →_ "
                        , " D  *  E "
                        , " F  _  H "
                        ]
                        |> Fixture.withGroup "ABC,F_H"
                    )
                    (Type 'C')
                    [ " A  B  C "
                    , " D  *  E "
                    , " F →_  H "
                    ]
        ]
