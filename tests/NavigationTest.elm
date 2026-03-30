module NavigationTest exposing (..)

import Crossword.Navigation.Guardian as Guardian
import Crossword.Navigation.NYT as NYT
import Crossword.Selection as Selection
import Crossword.Types exposing (Arrow(..), ClueId, Direction(..), Grid, Puzzle, Selection)
import Expect
import Fixture
import Test exposing (..)


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


-- Apply a navigation operation and compare the resulting selection against an expected grid.
navigates :
    List String
    -> (Grid -> Puzzle -> Selection -> Selection)
    -> List String
    -> Expect.Expectation
navigates inputRows navFn expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows
    in
    navFn fixture.grid fixture.puzzle fixture.selection
        |> Fixture.renderGrid fixture.puzzle fixture.grid
        |> Expect.equal expectedRows


-- Apply a selectClue operation and compare the resulting selection against an expected grid.
navigatesSelectClue :
    List String
    -> (Grid -> Puzzle -> ClueId -> Selection)
    -> ClueId
    -> List String
    -> Expect.Expectation
navigatesSelectClue inputRows selectFn cid expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows
    in
    selectFn fixture.grid fixture.puzzle cid
        |> Fixture.renderGrid fixture.puzzle fixture.grid
        |> Expect.equal expectedRows


-- Simulate a cell click (given col, row) and compare the resulting selection.
click :
    List String
    -> ( Int, Int )
    -> List String
    -> Expect.Expectation
click inputRows clickPos expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows

        result =
            Selection.selectCell clickPos (Just fixture.selection) fixture.puzzle
    in
    case result of
        Nothing ->
            Expect.fail "selectCell returned Nothing (clicked a black cell?)"

        Just sel ->
            Fixture.renderGrid fixture.puzzle fixture.grid sel
                |> Expect.equal expectedRows


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
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Guardian.strategy.nextClue
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "nextClue from the last clue wraps to the first" <|
            \_ ->
                -- 2-down is last in Guardian order; wraps back to 1-across
                navigates
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Guardian.strategy.nextClue
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "prevClue goes to the previous clue" <|
            \_ ->
                navigates
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
                    Guardian.strategy.prevClue
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "prevClue from the first clue wraps to the last" <|
            \_ ->
                -- 2-down is last in Guardian order
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Guardian.strategy.prevClue
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter advances one cell within the clue" <|
            \_ ->
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Guardian.strategy.afterLetter False)
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter at the last cell of a clue wraps to its first" <|
            \_ ->
                navigates
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (Guardian.strategy.afterLetter False)
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "selectClue always selects the first cell, ignoring blanks" <|
            \_ ->
                -- Guardian ignores blanks; always goes to cell index 0
                navigatesSelectClue
                    [ " A  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    Guardian.strategy.selectClue
                    { number = 1, direction = Across }
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
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
                    NYT.strategy.nextClue
                    [ " A  B  C "
                    , " D  *  E "
                    , "→_  G  H "
                    ]
        , test "nextClue stays put when every clue is filled" <|
            \_ ->
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    NYT.strategy.nextClue
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "prevClue goes to the first blank of the previous unfilled clue" <|
            \_ ->
                -- Starting at 2-down; going backward, 1-down has a blank at (0,2)
                navigates
                    [ " A  B ↓C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
                    NYT.strategy.prevClue
                    [ " A  B  C "
                    , " D  *  E "
                    , "↓_  G  H "
                    ]
        , test "afterLetter in a blank advances to the next blank in the clue" <|
            \_ ->
                -- A was blank; next blank in 1-across is the _ at (2,0)
                navigates
                    [ "→A  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (NYT.strategy.afterLetter True)
                    [ " A  B →_ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter at the last blank moves to the first blank of the next unfilled clue" <|
            \_ ->
                -- No blanks remain in 1-across after filling A; next unfilled is 3-across
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " _  G  H "
                    ]
                    (NYT.strategy.afterLetter True)
                    [ " A  B  C "
                    , " D  *  E "
                    , "→_  G  H "
                    ]
        , test "afterLetter on a filled cell advances to the next cell" <|
            \_ ->
                navigates
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (NYT.strategy.afterLetter False)
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "afterLetter at the last filled cell stays put" <|
            \_ ->
                navigates
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (NYT.strategy.afterLetter False)
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "selectClue with blanks selects the first blank, not the first cell" <|
            \_ ->
                navigatesSelectClue
                    [ " A  B  _ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    NYT.strategy.selectClue
                    { number = 1, direction = Across }
                    [ " A  B →_ "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "selectClue on a fully filled clue selects its first cell" <|
            \_ ->
                navigatesSelectClue
                    [ " A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    NYT.strategy.selectClue
                    { number = 1, direction = Across }
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
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowRight puzzle sel)
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowRight skips over black cells" <|
            \_ ->
                -- Row 1 white cells are [(0,1), (2,1)]; ArrowRight from D jumps to E
                navigates
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowRight puzzle sel)
                    [ " A  B  C "
                    , " D  * ↓E "
                    , " F  G  H "
                    ]
        , test "ArrowRight wraps from the last white cell in a row to the first" <|
            \_ ->
                navigates
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowRight puzzle sel)
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowLeft moves to the previous white cell in the row" <|
            \_ ->
                navigates
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowLeft puzzle sel)
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowDown moves to the next white cell in the column, preferring Down" <|
            \_ ->
                -- Col 0: [(0,0),(0,1),(0,2)]; ArrowDown from A lands on D in 1-down
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowDown puzzle sel)
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowUp moves to the previous white cell in the column, preferring Down" <|
            \_ ->
                -- Col 0: [(0,0),(0,1),(0,2)]; ArrowUp from D lands on A
                navigates
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowUp puzzle sel)
                    [ "↓A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "ArrowDown skips over black cells in a column" <|
            \_ ->
                -- Col 1: [(1,0),(1,2)]; ArrowDown from B skips black at (1,1) and lands on G
                navigates
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.arrowMove ArrowDown puzzle sel)
                    [ " A  B  C "
                    , " D  *  E "
                    , " F →G  H "
                    ]
        ]


-- Simulate a first click (no prior selection) and compare the resulting selection.
clickFresh :
    List String
    -> ( Int, Int )
    -> List String
    -> Expect.Expectation
clickFresh inputRows clickPos expectedRows =
    let
        fixture =
            Fixture.fromGrid inputRows

        result =
            Selection.selectCell clickPos Nothing fixture.puzzle
    in
    case result of
        Nothing ->
            Expect.fail "selectCell returned Nothing (clicked a black cell?)"

        Just sel ->
            Fixture.renderGrid fixture.puzzle fixture.grid sel
                |> Expect.equal expectedRows


clickSuite : Test
clickSuite =
    describe "Cell clicks"
        [ test "clicking a new cell moves the selection there" <|
            \_ ->
                click
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    ( 0, 2 )
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "clicking a cell that only belongs to a down clue selects it in down" <|
            \_ ->
                -- D at (0,1) is DownOnly; freshSelection falls back from Across to Down
                click
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    ( 0, 1 )
                    [ " A  B  C "
                    , "↓D  *  E "
                    , " F  G  H "
                    ]
        , test "clicking the selected cell again toggles the active direction" <|
            \_ ->
                -- Start on 1-across at A. Click A again → switches to 1-down.
                -- Consequence: Guardian Tab now goes to 2-down (C) instead of 3-across (F).
                let
                    fixture =
                        Fixture.fromGrid
                            [ "→A  B  C "
                            , " D  *  E "
                            , " F  G  H "
                            ]

                    toggled =
                        Selection.selectCell ( 0, 0 ) (Just fixture.selection) fixture.puzzle
                in
                case toggled of
                    Nothing ->
                        Expect.fail "expected a selection after click"

                    Just toggledSel ->
                        Guardian.strategy.nextClue fixture.grid fixture.puzzle toggledSel
                            |> Fixture.renderGrid fixture.puzzle fixture.grid
                            |> Expect.equal
                                [ " A  B ↓C "
                                , " D  *  E "
                                , " F  G  H "
                                ]
        , test "backspace on empty moves to the previous cell in the clue" <|
            \_ ->
                navigates
                    [ " A →B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.prevCell puzzle sel)
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "backspace at the first cell of a clue wraps to the last" <|
            \_ ->
                navigates
                    [ "→A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    (\_ puzzle sel -> Selection.prevCell puzzle sel)
                    [ " A  B →C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
        , test "clicking a black cell does not change the selection" <|
            \_ ->
                let
                    fixture =
                        Fixture.fromGrid
                            [ "→A  B  C "
                            , " D  *  E "
                            , " F  G  H "
                            ]
                in
                Selection.selectCell ( 1, 1 ) (Just fixture.selection) fixture.puzzle
                    |> Expect.equal (Just fixture.selection)
        , test "first click with no prior selection creates a fresh selection" <|
            \_ ->
                clickFresh
                    [ " A  B  C "
                    , " D  *  E "
                    , " F  G  H "
                    ]
                    ( 0, 2 )
                    [ " A  B  C "
                    , " D  *  E "
                    , "→F  G  H "
                    ]
        , test "clicking the selected cell again when it has only one direction does not toggle" <|
            \_ ->
                -- D at (0,1) is DownOnly; there is no Across clue to toggle to
                let
                    fixture =
                        Fixture.fromGrid
                            [ " A  B  C "
                            , "↓D  *  E "
                            , " F  G  H "
                            ]
                in
                Selection.selectCell ( 0, 1 ) (Just fixture.selection) fixture.puzzle
                    |> Expect.equal (Just fixture.selection)
        ]


groupSuite : Test
groupSuite =
    describe "Grouped (linked) clues"
        [ test "Guardian: typing at the last cell of the first linked clue continues into the second" <|
            \_ ->
                let
                    fixture =
                        Fixture.fromGrid
                            [ " A  B →C "
                            , " D  *  E "
                            , " F  G  H "
                            ]
                            |> Fixture.withGroup "ABC,FGH"
                in
                Guardian.strategy.afterLetter False fixture.grid fixture.puzzle fixture.selection
                    |> Fixture.renderGrid fixture.puzzle fixture.grid
                    |> Expect.equal
                        [ " A  B  C "
                        , " D  *  E "
                        , "→F  G  H "
                        ]
        , test "Guardian: typing at the last cell of the last linked clue wraps to the first" <|
            \_ ->
                let
                    fixture =
                        Fixture.fromGrid
                            [ " A  B  C "
                            , " D  *  E "
                            , " F  G →H "
                            ]
                            |> Fixture.withGroup "ABC,FGH"
                in
                Guardian.strategy.afterLetter False fixture.grid fixture.puzzle fixture.selection
                    |> Fixture.renderGrid fixture.puzzle fixture.grid
                    |> Expect.equal
                        [ "→A  B  C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        , test "Guardian: backspace at the first cell of the second linked clue goes to the last of the first" <|
            \_ ->
                let
                    fixture =
                        Fixture.fromGrid
                            [ " A  B  C "
                            , " D  *  E "
                            , "→F  G  H "
                            ]
                            |> Fixture.withGroup "ABC,FGH"
                in
                Selection.prevCell fixture.puzzle fixture.selection
                    |> Fixture.renderGrid fixture.puzzle fixture.grid
                    |> Expect.equal
                        [ " A  B →C "
                        , " D  *  E "
                        , " F  G  H "
                        ]
        , test "NYT: typing in a blank searches for the next blank across the whole linked group" <|
            \_ ->
                -- 1-across is fully filled; 3-across has a blank at F(0,2)
                -- Without grouping, afterLetter True would jump to a different clue.
                -- With grouping, it finds F as the next blank within the group.
                let
                    fixture =
                        Fixture.fromGrid
                            [ " A  B →C "
                            , " D  *  E "
                            , " _  G  H "
                            ]
                            |> Fixture.withGroup "ABC,_GH"
                in
                NYT.strategy.afterLetter True fixture.grid fixture.puzzle fixture.selection
                    |> Fixture.renderGrid fixture.puzzle fixture.grid
                    |> Expect.equal
                        [ " A  B  C "
                        , " D  *  E "
                        , "→_  G  H "
                        ]
        ]
