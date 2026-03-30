module Fixture exposing (Fixture, fromGrid, render, renderGrid, withGroup)

import Crossword.Decode exposing (buildCellInfos)
import Crossword.Grid as Grid
import Crossword.Selection as Selection
import Crossword.Types as Types
    exposing
        ( CellValue(..)
        , Clue
        , ClueId
        , Direction(..)
        , Grid
        , Position
        , Puzzle
        , Selection
        )
import Dict exposing (Dict)


type alias Fixture =
    { puzzle : Puzzle
    , grid : Grid
    , selection : Maybe Selection
    }


type alias WhiteCell =
    { value : Maybe Char
    , selectedDir : Maybe Direction
    }


fromGrid : List String -> Fixture
fromGrid rows =
    let
        rawCells : Dict Position (Maybe WhiteCell)
        rawCells =
            rows
                |> List.indexedMap parseRowStr
                |> List.foldl Dict.union Dict.empty

        numRows =
            List.length rows

        numCols =
            rawCells
                |> Dict.keys
                |> List.map Tuple.first
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 1

        isWhite : Position -> Bool
        isWhite pos =
            case Dict.get pos rawCells of
                Just (Just _) ->
                    True

                _ ->
                    False

        clues =
            deriveClues isWhite numCols numRows

        cellInfos =
            buildCellInfos clues

        grid : Grid
        grid =
            rawCells
                |> Dict.toList
                |> List.filterMap
                    (\( pos, maybeCell ) ->
                        maybeCell
                            |> Maybe.map
                                (\{ value } ->
                                    case value of
                                        Just ch ->
                                            ( pos, Filled ch )

                                        Nothing ->
                                            ( pos, Empty )
                                )
                    )
                |> Dict.fromList

        puzzle : Puzzle
        puzzle =
            { id = "fixture"
            , puzzleNumber = 0
            , name = "Fixture"
            , setter = Nothing
            , dimensions = { cols = numCols, rows = numRows }
            , crosswordType = "cryptic"
            , clues = clues
            , cellInfos = cellInfos
            }

        selectedInfo : Maybe ( Position, Direction )
        selectedInfo =
            rawCells
                |> Dict.toList
                |> List.filterMap
                    (\( pos, maybeCell ) ->
                        maybeCell
                            |> Maybe.andThen
                                (\{ selectedDir } ->
                                    Maybe.map (Tuple.pair pos) selectedDir
                                )
                    )
                |> List.head

        selection : Maybe Selection
        selection =
            selectedInfo
                |> Maybe.andThen
                    (\( pos, dir ) ->
                        let
                            maybeCid =
                                Dict.get pos cellInfos
                                    |> Maybe.andThen
                                        (\cellInfo ->
                                            case Types.clueIdForDirection dir cellInfo of
                                                Just cid ->
                                                    Just cid

                                                Nothing ->
                                                    Types.clueIdForDirection (Types.flipDirection dir) cellInfo
                                        )
                        in
                        maybeCid
                            |> Maybe.andThen
                                (\cid ->
                                    Types.lookupClue cid puzzle
                                        |> Maybe.map
                                            (\clue ->
                                                { clueId = cid
                                                , cellIndex = Grid.cellIndexFromPosition pos clue
                                                }
                                            )
                                )
                    )
    in
    { puzzle = puzzle, grid = grid, selection = selection }


renderGrid : Puzzle -> Grid -> Selection -> List String
renderGrid puzzle grid selection =
    let
        selectedPos =
            Selection.selectionPosition selection puzzle

        renderCell : Int -> Int -> String
        renderCell col row =
            case Dict.get ( col, row ) puzzle.cellInfos of
                Nothing ->
                    " * "

                Just _ ->
                    let
                        cellStr =
                            case Grid.get ( col, row ) grid of
                                Empty ->
                                    "_"

                                Filled ch ->
                                    String.fromChar ch
                    in
                    if selectedPos == Just ( col, row ) then
                        case selection.clueId.direction of
                            Across ->
                                "→" ++ cellStr ++ " "

                            Down ->
                                "↓" ++ cellStr ++ " "

                    else
                        " " ++ cellStr ++ " "
    in
    List.range 0 (puzzle.dimensions.rows - 1)
        |> List.map
            (\row ->
                List.range 0 (puzzle.dimensions.cols - 1)
                    |> List.map (\col -> renderCell col row)
                    |> String.concat
            )


render : Fixture -> Selection -> List String
render fixture sel =
    renderGrid fixture.puzzle fixture.grid sel


-- Internal: parse a row string into a dict of (col, row) -> Maybe WhiteCell
-- Nothing = black cell, Just = white cell
parseRowStr : Int -> String -> Dict Position (Maybe WhiteCell)
parseRowStr rowIdx str =
    parseRowChars rowIdx 0 (String.toList str) Dict.empty


parseRowChars : Int -> Int -> List Char -> Dict Position (Maybe WhiteCell) -> Dict Position (Maybe WhiteCell)
parseRowChars rowIdx col chars acc =
    case chars of
        [] ->
            acc

        a :: b :: c :: rest ->
            let
                cell =
                    case ( a, b, c ) of
                        ( '→', '_', ' ' ) ->
                            Just { value = Nothing, selectedDir = Just Across }

                        ( '→', ch, ' ' ) ->
                            Just { value = Just ch, selectedDir = Just Across }

                        ( '↓', '_', ' ' ) ->
                            Just { value = Nothing, selectedDir = Just Down }

                        ( '↓', ch, ' ' ) ->
                            Just { value = Just ch, selectedDir = Just Down }

                        ( ' ', '*', ' ' ) ->
                            Nothing

                        ( ' ', '_', ' ' ) ->
                            Just { value = Nothing, selectedDir = Nothing }

                        ( ' ', ch, ' ' ) ->
                            Just { value = Just ch, selectedDir = Nothing }

                        _ ->
                            Nothing
            in
            parseRowChars rowIdx (col + 1) rest (Dict.insert ( col, rowIdx ) cell acc)

        _ ->
            acc


-- Derive clue list from the grid white/black pattern
deriveClues : (Position -> Bool) -> Int -> Int -> List Clue
deriveClues isWhite numCols numRows =
    let
        allPositions =
            List.range 0 (numRows - 1)
                |> List.concatMap
                    (\row ->
                        List.range 0 (numCols - 1)
                            |> List.map (\col -> ( col, row ))
                    )

        startsAcross : Position -> Bool
        startsAcross ( col, row ) =
            isWhite ( col, row )
                && not (isWhite ( col - 1, row ))
                && isWhite ( col + 1, row )

        startsDown : Position -> Bool
        startsDown ( col, row ) =
            isWhite ( col, row )
                && not (isWhite ( col, row - 1 ))
                && isWhite ( col, row + 1 )

        ( _, numberedPositions ) =
            List.foldl
                (\pos ( nextNum, acc ) ->
                    if startsAcross pos || startsDown pos then
                        ( nextNum + 1, Dict.insert pos nextNum acc )

                    else
                        ( nextNum, acc )
                )
                ( 1, Dict.empty )
                allPositions

        countRun : ( Int, Int ) -> Position -> Int
        countRun ( dc, dr ) ( col, row ) =
            if isWhite ( col, row ) then
                1 + countRun ( dc, dr ) ( col + dc, row + dr )

            else
                0

        makeClue : Int -> Direction -> Position -> Int -> Clue
        makeClue num dir pos length =
            let
                cid =
                    { number = num, direction = dir }
            in
            { id = cid
            , humanNumber = String.fromInt num
            , text = ""
            , length = length
            , position = pos
            , separators = []
            , group = [ cid ]
            , solution = Nothing
            }

        numberedList =
            Dict.toList numberedPositions

        acrossClues =
            numberedList
                |> List.filterMap
                    (\( pos, num ) ->
                        if startsAcross pos then
                            Just (makeClue num Across pos (countRun ( 1, 0 ) pos))

                        else
                            Nothing
                    )
                |> List.sortBy (\c -> c.id.number)

        downClues =
            numberedList
                |> List.filterMap
                    (\( pos, num ) ->
                        if startsDown pos then
                            Just (makeClue num Down pos (countRun ( 0, 1 ) pos))

                        else
                            Nothing
                    )
                |> List.sortBy (\c -> c.id.number)
    in
    acrossClues ++ downClues


{- Link a set of clues into a single group so that nextCell/prevCell traverse
   them as one answer. The argument is a comma-separated list of clue answers
   as they appear in the grid, e.g. "ABC,FGH" or "A_C,_GH" for clues with
   empty cells. Case-insensitive. Clues not matched are left unchanged.
-}
withGroup : String -> Fixture -> Fixture
withGroup groupStr fixture =
    let
        targets =
            String.split "," groupStr |> List.map String.toUpper

        clueAnswer : Clue -> String
        clueAnswer clue =
            List.range 0 (clue.length - 1)
                |> List.map
                    (\idx ->
                        case Grid.get (Grid.positionFromCellIndex idx clue) fixture.grid of
                            Filled ch ->
                                String.fromChar (Char.toUpper ch)

                            Empty ->
                                "_"
                    )
                |> String.concat

        groupIds : List ClueId
        groupIds =
            fixture.puzzle.clues
                |> List.filterMap
                    (\clue ->
                        if List.member (clueAnswer clue) targets then
                            Just clue.id

                        else
                            Nothing
                    )

        puzzle =
            fixture.puzzle

        updatedPuzzle =
            { puzzle
                | clues =
                    List.map
                        (\clue ->
                            if List.member clue.id groupIds then
                                { clue | group = groupIds }

                            else
                                clue
                        )
                        puzzle.clues
            }
    in
    { fixture | puzzle = updatedPuzzle }
