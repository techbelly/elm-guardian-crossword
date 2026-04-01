module EntryTest exposing (suite)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Fixture
import Test exposing (Test, describe, test)
import TestHelpers exposing (UIAction(..), uitest, uitestFixture)


suite : Test
suite =
    describe "Entry"
        [ nytTypingSuite
        , guardianTypingSuite
        , backspaceSuite
        , groupSuite
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
        , test "advances one cell within the clue" <|
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
        , test "at the last filled cell stays put" <|
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
        , test "at the last blank moves to the first blank of the next unfilled clue" <|
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
        , test "advances one cell within the clue" <|
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
        , test "at the last cell wraps back to the first" <|
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
