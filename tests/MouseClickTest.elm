module MouseClickTest exposing (suite)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Types exposing (Direction(..))
import Test exposing (Test, describe, test)
import TestHelpers exposing (UIAction(..), uitest)


suite : Test
suite =
    describe "Mouse and Clue Selection"
        [ clueSelectionSuite
        , clickSuite
        ]


clueSelectionSuite : Test
clueSelectionSuite =
    describe "Clue selection"
        [ test "Guardian: always selects the first cell, ignoring blanks" <|
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
        , test "NYT: selects the first blank, not the first cell" <|
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
        , test "NYT: on a fully filled clue, selects the first cell" <|
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
