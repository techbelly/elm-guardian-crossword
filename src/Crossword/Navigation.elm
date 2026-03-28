module Crossword.Navigation exposing
    ( arrowMove
    , nextCell
    , nextClue
    , prevCell
    , prevClue
    )

import Crossword.Grid as Grid
import Crossword.Types as Types
    exposing
        ( Arrow(..)
        , Direction(..)
        , ClueId
        , Puzzle
        , Selection
        )
import Dict
import ListExtra


nextCell : Puzzle -> Selection -> Selection
nextCell puzzle sel =
    groupSelections sel.clueId puzzle
        |> ListExtra.nextInList sel
        |> Maybe.withDefault sel


prevCell : Puzzle -> Selection -> Selection
prevCell puzzle sel =
    groupSelections sel.clueId puzzle
        |> ListExtra.prevInList sel
        |> Maybe.withDefault sel


arrowMove : Arrow -> Puzzle -> Selection -> Selection
arrowMove arrow puzzle sel =
    case selectionPosition sel puzzle of
        Nothing ->
            sel

        Just currentPos ->
            let
                move positionsIn stepFn =
                    stepFn currentPos (positionsIn currentPos puzzle)

                ( target, preferredDir ) =
                    case arrow of
                        ArrowLeft ->
                            ( move rowPositions ListExtra.prevInList, Across )

                        ArrowRight ->
                            ( move rowPositions ListExtra.nextInList, Across )

                        ArrowUp ->
                            ( move colPositions ListExtra.prevInList, Down )

                        ArrowDown ->
                            ( move colPositions ListExtra.nextInList, Down )
            in
            case target of
                Just pos ->
                    selectionForPosition pos preferredDir puzzle
                        |> Maybe.withDefault sel

                Nothing ->
                    sel


nextClue : Puzzle -> Selection -> Selection
nextClue puzzle sel =
    puzzle.clues
        |> List.map .id
        |> ListExtra.nextInList sel.clueId
        |> Maybe.map (\cid -> { clueId = cid, cellIndex = 0 })
        |> Maybe.withDefault sel


prevClue : Puzzle -> Selection -> Selection
prevClue puzzle sel =
    puzzle.clues
        |> List.map .id
        |> ListExtra.prevInList sel.clueId
        |> Maybe.map (\cid -> { clueId = cid, cellIndex = 0 })
        |> Maybe.withDefault sel


-- Building selection lists


groupSelections : ClueId -> Puzzle -> List Selection
groupSelections clueId puzzle =
    case Types.lookupClue clueId puzzle of
        Nothing ->
            []

        Just clue ->
            clue.group
                |> List.concatMap (\cid -> clueSelections cid puzzle)


clueSelections : ClueId -> Puzzle -> List Selection
clueSelections cid puzzle =
    case Types.lookupClue cid puzzle of
        Nothing ->
            []

        Just clue ->
            List.range 0 (clue.length - 1)
                |> List.map (\idx -> { clueId = cid, cellIndex = idx })


rowPositions : Types.Position -> Puzzle -> List Types.Position
rowPositions ( _, row ) puzzle =
    puzzle.cellInfos
        |> Dict.keys
        |> List.filter (\( _, r ) -> r == row)
        |> List.sortBy Tuple.first


colPositions : Types.Position -> Puzzle -> List Types.Position
colPositions ( col, _ ) puzzle =
    puzzle.cellInfos
        |> Dict.keys
        |> List.filter (\( c, _ ) -> c == col)
        |> List.sortBy Tuple.second


-- Position and clue resolution


selectionPosition : Selection -> Puzzle -> Maybe Types.Position
selectionPosition sel puzzle =
    Types.lookupClue sel.clueId puzzle
        |> Maybe.map (Grid.positionFromCellIndex sel.cellIndex)


selectionForPosition : Types.Position -> Types.Direction -> Puzzle -> Maybe Selection
selectionForPosition pos preferredDir puzzle =
    Dict.get pos puzzle.cellInfos
        |> Maybe.andThen (selectionFromCellInfo pos preferredDir puzzle)


selectionFromCellInfo : Types.Position -> Types.Direction -> Puzzle -> Types.CellInfo -> Maybe Selection
selectionFromCellInfo pos preferredDir puzzle cellInfo =
    let
        first =
            Types.clueIdForDirection preferredDir cellInfo

        second =
            Types.clueIdForDirection (Types.flipDirection preferredDir) cellInfo
    in
    case first of
        Just cid ->
            selectionForClueId pos cid puzzle

        Nothing ->
            Maybe.andThen (\cid -> selectionForClueId pos cid puzzle) second


selectionForClueId : Types.Position -> ClueId -> Puzzle -> Maybe Selection
selectionForClueId pos cid puzzle =
    Types.lookupClue cid puzzle
        |> Maybe.map
            (\clue ->
                { clueId = cid
                , cellIndex = Grid.cellIndexFromPosition pos clue
                }
            )
