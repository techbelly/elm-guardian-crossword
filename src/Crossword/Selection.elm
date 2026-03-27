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
            if selectionPosition puzzle sel == Just clickPos then
                toggleDirection clickPos cellInfo sel puzzle

            else if cellInActiveGroup clickPos sel puzzle then
                moveCursorWithinClue clickPos sel puzzle
                    |> Maybe.withDefault (freshSelection clickPos cellInfo puzzle)

            else
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
    let
        currentDir =
            sel.clueId.direction

        newDir =
            Types.flipDirection currentDir

        hasNewDir =
            case newDir of
                Across ->
                    Types.acrossClueId cellInfo /= Nothing

                Down ->
                    Types.downClueId cellInfo /= Nothing
    in
    if hasNewDir then
        let
            newClueId =
                case newDir of
                    Across ->
                        Maybe.withDefault sel.clueId (Types.acrossClueId cellInfo)

                    Down ->
                        Maybe.withDefault sel.clueId (Types.downClueId cellInfo)
        in
        withClue newClueId puzzle
            (\clue ->
                Just
                    { clueId = newClueId
                    , cellIndex = computeCellIndex cellPos clue
                    }
            )
            |> Maybe.withDefault sel

    else
        sel


pickClueAndIndex : Types.Position -> Direction -> CellInfo -> Puzzle -> Selection
pickClueAndIndex cellPos preferredDir cellInfo puzzle =
    let
        tryClue : Maybe ClueId -> Maybe Selection
        tryClue maybeClueId =
            maybeClueId
                |> Maybe.andThen
                    (\cid ->
                        withClue cid puzzle
                            (\clue ->
                                Just
                                    { clueId = cid
                                    , cellIndex = computeCellIndex cellPos clue
                                    }
                            )
                    )

        fallback =
            { clueId = { number = 0, direction = Across }, cellIndex = 0 }
    in
    case preferredDir of
        Across ->
            tryClue (Types.acrossClueId cellInfo)
                |> Maybe.withDefault
                    (tryClue (Types.downClueId cellInfo)
                        |> Maybe.withDefault fallback
                    )

        Down ->
            tryClue (Types.downClueId cellInfo)
                |> Maybe.withDefault
                    (tryClue (Types.acrossClueId cellInfo)
                        |> Maybe.withDefault fallback
                    )


computeCellIndex : Types.Position -> Clue -> Int
computeCellIndex ( c, r ) clue =
    let
        ( clueCol, clueRow ) =
            clue.position
    in
    case clue.id.direction of
        Across ->
            c - clueCol

        Down ->
            r - clueRow


cellInActiveGroup : Types.Position -> Selection -> Puzzle -> Bool
cellInActiveGroup cellPos sel puzzle =
    moveCursorWithinClue cellPos sel puzzle /= Nothing


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
            List.range 0 (clue.length - 1)
                |> List.filter
                    (\idx -> Grid.clueCell idx clue == pos)
                |> List.head
                |> Maybe.map (\idx -> { clueId = clueId, cellIndex = idx })
        )


selectionPosition : Puzzle -> Selection -> Maybe Types.Position
selectionPosition puzzle sel =
    withClue sel.clueId puzzle
        (Grid.clueCell sel.cellIndex >> Just)


withClue : ClueId -> Puzzle -> (Clue -> Maybe a) -> Maybe a
withClue clueId puzzle f =
    puzzle.clues
        |> List.filter (\c -> c.id == clueId)
        |> List.head
        |> Maybe.andThen f
