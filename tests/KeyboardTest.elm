module KeyboardTest exposing (..)

import Crossword.Keyboard as Keyboard
import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Types exposing (ActiveModel, NavigationStrategy, NavigationStyle(..))
import Expect
import Fixture
import Test exposing (..)


toModel : Fixture.Fixture -> ActiveModel
toModel fixture =
    { puzzle = fixture.puzzle
    , grid = fixture.grid
    , selection = Just fixture.selection
    , navigationStyle = NYT
    }


typesLetter : NavigationStrategy -> List String -> Char -> List String -> Expect.Expectation
typesLetter strategy inputRows ch expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows

        ( newModel, _ ) =
            Keyboard.handleLetter strategy ch fixture.selection (toModel fixture)
    in
    case newModel.selection of
        Nothing ->
            Expect.fail "Expected a selection after typing"

        Just sel ->
            Fixture.renderGrid newModel.puzzle newModel.grid sel
                |> Expect.equal expectedRows


pressesBackspace : List String -> List String -> Expect.Expectation
pressesBackspace inputRows expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows

        ( newModel, _ ) =
            Keyboard.handleBackspace fixture.puzzle fixture.selection (toModel fixture)
    in
    case newModel.selection of
        Nothing ->
            Expect.fail "Expected a selection after backspace"

        Just sel ->
            Fixture.renderGrid newModel.puzzle newModel.grid sel
                |> Expect.equal expectedRows


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
