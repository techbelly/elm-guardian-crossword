module KeyboardNavTest exposing (suite)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Types exposing (Arrow(..))
import Test exposing (Test, describe, test)
import TestHelpers exposing (UIAction(..), uitest)


suite : Test
suite =
    describe "Keyboard Navigation"
        [ guardianTabSuite
        , nytTabSuite
        , arrowSuite
        ]


guardianTabSuite : Test
guardianTabSuite =
    describe "Guardian"
        [ test "Tab advances to the next clue" <|
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
        , test "Tab from the last clue wraps to the first" <|
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
        , test "ShiftTab goes to the previous clue" <|
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
        , test "ShiftTab from the first clue wraps to the last" <|
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
        ]


nytTabSuite : Test
nytTabSuite =
    describe "NYT"
        [ test "Tab skips filled clues and lands on the first blank of the next unfilled clue" <|
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
        , test "Tab stays put when every clue is filled" <|
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
        , test "ShiftTab goes to the first blank of the previous unfilled clue" <|
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
