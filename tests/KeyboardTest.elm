module KeyboardTest exposing (..)

import Crossword.Keyboard as Keyboard
import Crossword.Navigation.NYT as NYT
import Crossword.Types exposing (ActiveModel, NavigationStyle(..))
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


typesLetter : List String -> Char -> List String -> Expect.Expectation
typesLetter inputRows ch expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows

        ( newModel, _ ) =
            Keyboard.handleLetter NYT.strategy ch fixture.selection (toModel fixture)
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
        [ describe "handleLetter"
            [ test "fills an empty cell and advances to the next blank (NYT)" <|
                \_ ->
                    typesLetter
                        [ "→_  _  _ "
                        , " D  *  E "
                        , " F  G  H "
                        ]
                        'A'
                        [ " A →_  _ "
                        , " D  *  E "
                        , " F  G  H "
                        ]
            , test "overwrites a filled cell and advances to the next cell (NYT)" <|
                \_ ->
                    typesLetter
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
        , describe "handleBackspace"
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
            ]
        ]
