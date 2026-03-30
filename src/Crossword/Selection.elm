module Crossword.Selection exposing
    ( arrowMove
    , clueSelections
    , groupSelections
    , nextCell
    , prevCell
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
            selectKnownCell clickPos cellInfo currentSelection puzzle


selectKnownCell : Types.Position -> CellInfo -> Maybe Selection -> Puzzle -> Maybe Selection
selectKnownCell clickPos cellInfo currentSelection puzzle =
    case currentSelection of
        Nothing ->
            freshSelection clickPos cellInfo puzzle

        Just sel ->
            if selectionPosition sel puzzle == Just clickPos then
                Just (toggleDirection clickPos cellInfo sel puzzle)

            else
                case findInGroup clickPos sel puzzle of
                    Just newSel ->
                        Just newSel

                    Nothing ->
                        freshSelection clickPos cellInfo puzzle


freshSelection : Types.Position -> CellInfo -> Puzzle -> Maybe Selection
freshSelection cellPos cellInfo puzzle =
    let
        direction =
            case cellInfo.start of
                StartsDown _ ->
                    Down

                _ ->
                    Across
    in
    selectionFromCellInfo cellPos direction puzzle cellInfo


toggleDirection : Types.Position -> CellInfo -> Selection -> Puzzle -> Selection
toggleDirection cellPos cellInfo sel puzzle =
    case Types.clueIdForDirection (Types.flipDirection sel.clueId.direction) cellInfo of
        Nothing ->
            sel

        Just newClueId ->
            selectionForClueId cellPos newClueId puzzle
                |> Maybe.withDefault sel


findInGroup : Types.Position -> Selection -> Puzzle -> Maybe Selection
findInGroup cellPos sel puzzle =
    Types.lookupClue sel.clueId puzzle
        |> Maybe.andThen
            (\clue ->
                clue.group
                    |> List.filterMap (\cid -> selectionIfInClue cellPos cid puzzle)
                    |> List.head
            )


selectionIfInClue : Types.Position -> ClueId -> Puzzle -> Maybe Selection
selectionIfInClue (( c, r ) as pos) clueId puzzle =
    Types.lookupClue clueId puzzle
        |> Maybe.andThen
            (\clue ->
                let
                    ( clueCol, clueRow ) =
                        clue.position

                    idx =
                        Grid.cellIndexFromPosition pos clue

                    onLine =
                        case clue.id.direction of
                            Types.Across ->
                                r == clueRow

                            Types.Down ->
                                c == clueCol
                in
                if onLine && idx >= 0 && idx < clue.length then
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
    let
        tryDir dir =
            Types.clueIdForDirection dir cellInfo
                |> Maybe.andThen (\cid -> selectionForClueId pos cid puzzle)
    in
    case tryDir preferredDir of
        Nothing ->
            tryDir (Types.flipDirection preferredDir)

        result ->
            result


selectionForClueId : Types.Position -> ClueId -> Puzzle -> Maybe Selection
selectionForClueId pos cid puzzle =
    Types.lookupClue cid puzzle
        |> Maybe.map (\clue -> { clueId = cid, cellIndex = Grid.cellIndexFromPosition pos clue })
