module Crossword.Selection exposing (selectCell)

import Crossword.Grid as Grid
import Crossword.Types as Types
    exposing
        ( CellInfo
        , ClueStart(..)
        , Direction(..)
        , Clue
        , ClueId
        , Puzzle
        , Selection
        )
import Dict


selectCell : Types.Position -> Maybe Selection -> Puzzle -> Maybe Selection
selectCell clickPos currentSelection puzzle =
    case Dict.get clickPos puzzle.cellInfos of
        Nothing ->
            currentSelection

        Just cellInfo ->
            Just (selectKnownCell clickPos cellInfo currentSelection puzzle)


selectKnownCell : Types.Position -> CellInfo -> Maybe Selection -> Puzzle -> Selection
selectKnownCell clickPos cellInfo currentSelection puzzle =
    case currentSelection of
        Nothing ->
            freshSelection clickPos cellInfo puzzle

        Just sel ->
            if selectionPosition sel puzzle == Just clickPos then
                toggleDirection clickPos cellInfo sel puzzle

            else
                case moveCursorWithinClue clickPos sel puzzle of
                    Just newSel ->
                        newSel

                    Nothing ->
                        freshSelection clickPos cellInfo puzzle


freshSelection : Types.Position -> CellInfo -> Puzzle -> Selection
freshSelection cellPos cellInfo puzzle =
    let
        direction =
            case cellInfo.start of
                StartsDown _ ->
                    Down

                _ ->
                    Across
    in
    pickClueAndIndex cellPos direction cellInfo puzzle


toggleDirection : Types.Position -> CellInfo -> Selection -> Puzzle -> Selection
toggleDirection cellPos cellInfo sel puzzle =
    case Types.clueIdForDirection (Types.flipDirection sel.clueId.direction) cellInfo of
        Nothing ->
            sel

        Just newClueId ->
            withClue newClueId puzzle
                (\clue -> Just { clueId = newClueId, cellIndex = Grid.cellIndexFromPosition cellPos clue })
                |> Maybe.withDefault sel


pickClueAndIndex : Types.Position -> Direction -> CellInfo -> Puzzle -> Selection
pickClueAndIndex cellPos preferredDir cellInfo puzzle =
    let
        tryClue maybeClueId =
            maybeClueId
                |> Maybe.andThen
                    (\cid ->
                        withClue cid puzzle
                            (\clue -> Just { clueId = cid, cellIndex = Grid.cellIndexFromPosition cellPos clue })
                    )

        fallback =
            { clueId = { number = 0, direction = Across }, cellIndex = 0 }
    in
    tryClue (Types.clueIdForDirection preferredDir cellInfo)
        |> Maybe.withDefault
            (tryClue (Types.clueIdForDirection (Types.flipDirection preferredDir) cellInfo)
                |> Maybe.withDefault fallback
            )


moveCursorWithinClue : Types.Position -> Selection -> Puzzle -> Maybe Selection
moveCursorWithinClue cellPos sel puzzle =
    withClue sel.clueId puzzle
        (\clue ->
            clue.group
                |> List.filterMap (\cid -> selectionAtPosition cellPos cid puzzle)
                |> List.head
        )


selectionAtPosition : Types.Position -> Types.ClueId -> Puzzle -> Maybe Selection
selectionAtPosition pos clueId puzzle =
    withClue clueId puzzle
        (\clue ->
            let
                idx =
                    Grid.cellIndexFromPosition pos clue
            in
            if idx >= 0 && idx < clue.length then
                Just { clueId = clueId, cellIndex = idx }

            else
                Nothing
        )


selectionPosition : Selection -> Puzzle -> Maybe Types.Position
selectionPosition sel puzzle =
    withClue sel.clueId puzzle (Grid.positionFromCellIndex sel.cellIndex >> Just)


withClue : ClueId -> Puzzle -> (Clue -> Maybe a) -> Maybe a
withClue cid puzzle f =
    Types.lookupClue cid puzzle |> Maybe.andThen f
