module Crossword.Navigation.NYT exposing (strategy)

import Crossword.Grid as Grid
import Crossword.Types exposing (NavigationStrategy)
import Crossword.Selection as Selection
import Crossword.Types as Types exposing (CellValue(..), Grid, Puzzle, Selection)
import ListExtra


strategy : NavigationStrategy
strategy =
    { afterLetter = afterLetter
    , nextClue = nextUnfilledClue
    , prevClue = prevUnfilledClue
    , selectClue = firstBlankInClue
    }


afterLetter : Bool -> Grid -> Puzzle -> Selection -> Selection
afterLetter wasBlank grid puzzle sel =
    if wasBlank then
        nextBlankInGroup grid puzzle sel

    else if isLastInGroup puzzle sel then
        sel

    else
        Selection.nextCell puzzle sel


nextBlankInGroup : Grid -> Puzzle -> Selection -> Selection
nextBlankInGroup grid puzzle sel =
    let
        allGroupCells =
            Selection.groupSelections sel.clueId puzzle

        selIdx =
            ListExtra.findIndex sel allGroupCells
                |> Maybe.withDefault -1

        blanksAfterCurrent =
            List.drop (selIdx + 1) allGroupCells
                |> List.filter (isBlank grid puzzle)
    in
    case blanksAfterCurrent of
        first :: _ ->
            first

        [] ->
            nextUnfilledClue grid puzzle sel


isBlank : Grid -> Puzzle -> Selection -> Bool
isBlank grid puzzle sel =
    Selection.selectionPosition sel puzzle
        |> Maybe.map (\pos -> Grid.get pos grid == Empty)
        |> Maybe.withDefault False


isLastInGroup : Puzzle -> Selection -> Bool
isLastInGroup puzzle sel =
    Selection.groupSelections sel.clueId puzzle
        |> List.reverse
        |> List.head
        |> Maybe.map (\last -> last == sel)
        |> Maybe.withDefault False


nextUnfilledClue : Grid -> Puzzle -> Selection -> Selection
nextUnfilledClue grid puzzle sel =
    unfilledClueFrom grid puzzle sel (List.map .id puzzle.clues)


prevUnfilledClue : Grid -> Puzzle -> Selection -> Selection
prevUnfilledClue grid puzzle sel =
    unfilledClueFrom grid puzzle sel (List.reverse (List.map .id puzzle.clues))


unfilledClueFrom : Grid -> Puzzle -> Selection -> List Types.ClueId -> Selection
unfilledClueFrom grid puzzle sel clueIds =
    let
        hasBlank cid =
            Selection.clueSelections cid puzzle
                |> List.any (isBlank grid puzzle)

        currentIdx =
            ListExtra.findIndex sel.clueId clueIds
                |> Maybe.withDefault 0

        cycled =
            List.drop (currentIdx + 1) clueIds
                ++ List.take currentIdx clueIds
    in
    cycled
        |> List.filter hasBlank
        |> List.head
        |> Maybe.map (firstBlankInClue grid puzzle)
        |> Maybe.withDefault sel


firstBlankInClue : Grid -> Puzzle -> Types.ClueId -> Selection
firstBlankInClue grid puzzle cid =
    Selection.clueSelections cid puzzle
        |> List.filter (isBlank grid puzzle)
        |> List.head
        |> Maybe.withDefault (Selection.selectClue cid)
