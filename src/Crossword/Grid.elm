module Crossword.Grid exposing
    ( get
    , isClueAnswered
    , clueCell
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


clueCell : Int -> Clue -> Position
clueCell idx clue =
    let
        ( col, row ) =
            clue.position
    in
    case clue.id.direction of
        Across ->
            ( col + idx, row )

        Down ->
            ( col, row + idx )


isClueAnswered : Clue -> Grid -> Bool
isClueAnswered clue grid =
    List.range 0 (clue.length - 1)
        |> List.all
            (\idx ->
                case get (clueCell idx clue) grid of
                    Filled _ ->
                        True

                    Empty ->
                        False
            )
