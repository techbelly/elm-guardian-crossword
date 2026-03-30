module KeyboardTest exposing (suite)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Test exposing (Test, describe, test)
import TestHelpers exposing (pressesBackspace, typesLetter)


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
                typesLetter NYT.strategy
                    [ "→_  _  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    'A'
                    [ " A →_  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "skips over a filled cell to reach the next blank" <|
            \_ ->
                typesLetter NYT.strategy
                    [ "→_  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    'A'
                    [ " A  B →_ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "overwrites a filled cell and advances to the next cell" <|
            \_ ->
                typesLetter NYT.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    'Z'
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
                typesLetter Guardian.strategy
                    [ "→_  _  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    'A'
                    [ " A →_  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "advances one cell even when the next cell is filled" <|
            \_ ->
                typesLetter Guardian.strategy
                    [ "→_  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    'A'
                    [ " A →B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "overwrites a filled cell and advances to the next cell" <|
            \_ ->
                typesLetter Guardian.strategy
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    'Z'
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
                pressesBackspace
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "clears a filled cell mid-clue and stays in place" <|
            \_ ->
                pressesBackspace
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    [ " A →_  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "moves to the previous cell when current is empty" <|
            \_ ->
                pressesBackspace
                    [ " A →_  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    [ "→A  _  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "stays put when at the first cell and it is empty" <|
            \_ ->
                pressesBackspace
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    [ "→_  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        ]
