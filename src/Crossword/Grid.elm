module Crossword.Grid exposing
    ( get
    , isClueAnswered
    , cellIndexFromPosition
    , positionFromCellIndex
    , set
    )

import Crossword.Types
    exposing
        ( CellValue(..)
        , Direction(..)
        , Clue
        , Grid
        , Position
        )
import Dict


get : Position -> Grid -> CellValue
get pos grid =
    Dict.get pos grid
        |> Maybe.withDefault Empty


set : Position -> CellValue -> Grid -> Grid
set pos value grid =
    Dict.insert pos value grid


positionFromCellIndex : Int -> Clue -> Position
positionFromCellIndex idx clue =
    let
        ( col, row ) =
            clue.position
    in
    case clue.id.direction of
        Across ->
            ( col + idx, row )

        Down ->
            ( col, row + idx )


cellIndexFromPosition : Position -> Clue -> Int
cellIndexFromPosition ( c, r ) clue =
    let
        ( clueCol, clueRow ) =
            clue.position
    in
    case clue.id.direction of
        Across ->
            c - clueCol

        Down ->
            r - clueRow


isClueAnswered : Clue -> Grid -> Bool
isClueAnswered clue grid =
    List.range 0 (clue.length - 1)
        |> List.all (\idx -> get (positionFromCellIndex idx clue) grid /= Empty)
