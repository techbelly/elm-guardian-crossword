module Crossword.Navigation.Guardian exposing (strategy)

import Crossword.Selection as Selection
import Crossword.Types exposing (NavigationStrategy, Puzzle, Selection)
import ListExtra


strategy : NavigationStrategy
strategy =
    { afterLetter = \_ _ puzzle sel -> Selection.nextCell puzzle sel
    , nextClue = \_ puzzle sel -> nextClue puzzle sel
    , prevClue = \_ puzzle sel -> prevClue puzzle sel
    , selectClue = \_ _ cid -> Selection.selectClue cid
    }


nextClue : Puzzle -> Selection -> Selection
nextClue puzzle sel =
    puzzle.clues
        |> List.map .id
        |> ListExtra.nextInList sel.clueId
        |> Maybe.map Selection.selectClue
        |> Maybe.withDefault sel


prevClue : Puzzle -> Selection -> Selection
prevClue puzzle sel =
    puzzle.clues
        |> List.map .id
        |> ListExtra.prevInList sel.clueId
        |> Maybe.map Selection.selectClue
        |> Maybe.withDefault sel
