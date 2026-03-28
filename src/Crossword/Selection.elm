module Crossword.Selection exposing
    ( arrowMove
    , nextCell
    , nextClue
    , prevCell
    , prevClue
    , selectCell
    , selectClue
    , selectionPosition
    )

import Crossword.Grid as Grid
import Crossword.Types as Types
    exposing
        ( Arrow(..)
        , CellInfo
        , ClueStart(..)
        , Direction(..)
        , ClueId
        , Puzzle
        , Selection
        )
import Dict
import ListExtra


selectClue : ClueId -> Selection
selectClue cid =
    { clueId = cid, cellIndex = 0 }


-- Click handling


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
            selectionForClueId cellPos newClueId puzzle
                |> Maybe.withDefault sel


pickClueAndIndex : Types.Position -> Direction -> CellInfo -> Puzzle -> Selection
pickClueAndIndex cellPos preferredDir cellInfo puzzle =
    let
        tryClue maybeClueId =
            maybeClueId |> Maybe.andThen (\cid -> selectionForClueId cellPos cid puzzle)

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
    Types.lookupClue sel.clueId puzzle
        |> Maybe.andThen
            (\clue ->
                clue.group
                    |> List.filterMap (\cid -> selectionAtPosition cellPos cid puzzle)
                    |> List.head
            )


selectionAtPosition : Types.Position -> ClueId -> Puzzle -> Maybe Selection
selectionAtPosition pos clueId puzzle =
    Types.lookupClue clueId puzzle
        |> Maybe.andThen
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


-- Keyboard handling


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


selectionForPosition : Types.Position -> Direction -> Puzzle -> Maybe Selection
selectionForPosition pos preferredDir puzzle =
    Dict.get pos puzzle.cellInfos
        |> Maybe.andThen (selectionFromCellInfo pos preferredDir puzzle)


selectionFromCellInfo : Types.Position -> Direction -> Puzzle -> Types.CellInfo -> Maybe Selection
selectionFromCellInfo pos preferredDir puzzle cellInfo =
    case Types.clueIdForDirection preferredDir cellInfo of
        Just cid ->
            selectionForClueId pos cid puzzle

        Nothing ->
            Types.clueIdForDirection (Types.flipDirection preferredDir) cellInfo
                |> Maybe.andThen (\cid -> selectionForClueId pos cid puzzle)


selectionForClueId : Types.Position -> ClueId -> Puzzle -> Maybe Selection
selectionForClueId pos cid puzzle =
    Types.lookupClue cid puzzle
        |> Maybe.map (\clue -> { clueId = cid, cellIndex = Grid.cellIndexFromPosition pos clue })
