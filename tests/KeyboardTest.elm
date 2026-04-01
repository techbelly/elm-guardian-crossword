module KeyboardTest exposing (suite)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Test exposing (Test, describe, test)
import TestHelpers exposing (uitest, UIAction(..))


suite : Test
suite =
    describe "Keyboard"
        [ nytTypingSuite
        , guardianTypingSuite
        , backspaceSuite
        ]


nytTypingSuite : Test
nytTypingSuite =
    describe "NYT typing"
        [ test "fills an empty cell and advances to the next blank" <|
            \_ ->
                uitest NYT.strategy
                    [ "→_  _  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'A')
                    [ " A →_  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "skips over a filled cell to reach the next blank" <|
            \_ ->
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
        , test "overwrites a filled cell and advances to the next cell" <|
            \_ ->
                uitest NYT.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'Z')
                    [ " A  Z →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        ]


guardianTypingSuite : Test
guardianTypingSuite =
    describe "Guardian typing"
        [ test "fills an empty cell and advances to the next cell" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→_  _  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'A')
                    [ " A →_  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "advances one cell even when the next cell is filled" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→_  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'A')
                    [ " A →B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "overwrites a filled cell and advances to the next cell" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Type 'Z')
                    [ " A  Z →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        ]


backspaceSuite : Test
backspaceSuite =
    describe "Backspace"
        [ test "clears a filled cell and stays in place" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Backspace
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "clears a filled cell mid-clue and stays in place" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Backspace
                    [ " A →_  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "moves to the previous cell when current is empty" <|
            \_ ->
                uitest Guardian.strategy
                    [ " A →_  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Backspace
                    [ "→A  _  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "stays put when at the first cell and it is empty" <|
            \_ ->
                uitest Guardian.strategy
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Backspace
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        ]
