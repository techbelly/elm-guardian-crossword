module TestHelpers exposing
    ( clicks
    , navigates
    , pressesBackspace
    , selectsClue
    , typesLetter
    , withSelection
    )

import Crossword.Keyboard as Keyboard
import Crossword.Selection as Selection
import Crossword.Types
    exposing
        ( ActiveModel
        , ClueId
        , Grid
        , NavigationStrategy
        , NavigationStyle(..)
        , Puzzle
        , Selection
        )
import Expect
import Fixture


navigates :
    List String
    -> (Grid -> Puzzle -> Selection -> Selection)
    -> List String
    -> Expect.Expectation
navigates inputRows navFn expectedRows =
    withSelection (Fixture.fromGrid inputRows)
        (\fixture sel ->
            navFn fixture.grid fixture.puzzle sel
                |> Fixture.render fixture
                |> Expect.equal expectedRows
        )


selectsClue :
    List String
    -> (Grid -> Puzzle -> ClueId -> Selection)
    -> ClueId
    -> List String
    -> Expect.Expectation
selectsClue inputRows selectFn cid expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows
    in
    selectFn fixture.grid fixture.puzzle cid
        |> Fixture.render fixture
        |> Expect.equal expectedRows


clicks :
    List String
    -> ( Int, Int )
    -> List String
    -> Expect.Expectation
clicks inputRows clickPos expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows
    in
    case Selection.selectCell clickPos fixture.selection fixture.puzzle of
        Nothing ->
            Expect.fail "selectCell returned Nothing (clicked a black cell?)"

        Just sel ->
            Fixture.render fixture sel
                |> Expect.equal expectedRows


typesLetter : NavigationStrategy -> List String -> Char -> List String -> Expect.Expectation
typesLetter strategy inputRows ch expectedRows =
    withSelection (Fixture.fromGrid inputRows)
        (\fixture sel ->
            Keyboard.handleLetter strategy ch sel (toModel fixture)
                |> Tuple.first
                |> afterKeyOp expectedRows
        )


pressesBackspace : List String -> List String -> Expect.Expectation
pressesBackspace inputRows expectedRows =
    withSelection (Fixture.fromGrid inputRows)
        (\fixture sel ->
            Keyboard.handleBackspace fixture.puzzle sel (toModel fixture)
                |> Tuple.first
                |> afterKeyOp expectedRows
        )


withSelection : Fixture.Fixture -> (Fixture.Fixture -> Selection -> Expect.Expectation) -> Expect.Expectation
withSelection fixture f =
    case fixture.selection of
        Nothing ->
            Expect.fail "no selection marker in input grid"

        Just sel ->
            f fixture sel


afterKeyOp : List String -> ActiveModel -> Expect.Expectation
afterKeyOp expectedRows newModel =
    case newModel.selection of
        Nothing ->
            Expect.fail "Expected a selection after key press"

        Just sel ->
            Fixture.renderGrid newModel.puzzle newModel.grid sel
                |> Expect.equal expectedRows


toModel : Fixture.Fixture -> ActiveModel
toModel fixture =
    { puzzle = fixture.puzzle
    , grid = fixture.grid
    , selection = fixture.selection
    , navigationStyle = NYT
    }
