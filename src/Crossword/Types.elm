module Crossword.Types exposing
    ( ActiveModel
    , CellClues(..)
    , ClueStart(..)
    , CellInfo
    , CellSeparator
    , CellValue(..)
    , Direction(..)
    , Clue
    , ClueId
    , Grid
    , Arrow(..)
    , Model(..)
    , Msg(..)
    , NavigationStrategy
    , NavigationStyle(..)
    , Position
    , Puzzle
    , Selection
    , ClueSeparator(..)
    , SeparatorKind(..)
    , clueIdForDirection
    , flipDirection
    , lookupClue
    )

import Dict exposing (Dict)


type NavigationStyle
    = Guardian
    | NYT


type alias NavigationStrategy =
    { afterLetter : Bool -> Grid -> Puzzle -> Selection -> Selection
    , nextClue : Grid -> Puzzle -> Selection -> Selection
    , prevClue : Grid -> Puzzle -> Selection -> Selection
    , selectClue : Grid -> Puzzle -> ClueId -> Selection
    }


type Arrow
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown


type Direction
    = Across
    | Down


flipDirection : Direction -> Direction
flipDirection dir =
    case dir of
        Across ->
            Down

        Down ->
            Across



type alias Position =
    ( Int, Int )



-- Clue identifier. Plain record so == works naturally.


type alias ClueId =
    { number : Int, direction : Direction }



-- Cell value: exactly one character or empty. Not Maybe String.


type CellValue
    = Empty
    | Filled Char



-- ClueSeparators parsed from JSON at the boundary.


type ClueSeparator
    = WordBoundary Int
    | Hyphen Int



-- Per-cell separator info for rendering.


type SeparatorKind
    = Bar
    | Dash


type alias CellSeparator =
    { direction : Direction
    , kind : SeparatorKind
    }



type alias Clue =
    { id : ClueId
    , humanNumber : String
    , text : String
    , length : Int
    , position : Position
    , separators : List ClueSeparator
    , group : List ClueId
    , solution : Maybe String
    }



type CellClues
    = AcrossOnly ClueId
    | DownOnly ClueId
    | AcrossAndDown { across : ClueId, down : ClueId }


acrossClueId : CellInfo -> Maybe ClueId
acrossClueId info =
    case info.clues of
        AcrossOnly cid ->
            Just cid

        AcrossAndDown e ->
            Just e.across

        DownOnly _ ->
            Nothing


downClueId : CellInfo -> Maybe ClueId
downClueId info =
    case info.clues of
        DownOnly cid ->
            Just cid

        AcrossAndDown e ->
            Just e.down

        AcrossOnly _ ->
            Nothing


clueIdForDirection : Direction -> CellInfo -> Maybe ClueId
clueIdForDirection dir info =
    case dir of
        Across ->
            acrossClueId info

        Down ->
            downClueId info


lookupClue : ClueId -> Puzzle -> Maybe Clue
lookupClue cid puzzle =
    puzzle.clues
        |> List.filter (\c -> c.id == cid)
        |> List.head



-- Whether this cell starts a clue, and if so which direction(s) and what number.


type ClueStart
    = NotStart
    | StartsAcross Int
    | StartsDown Int
    | StartsBoth Int



-- Pre-computed metadata for each editable cell, built once at parse time.


type alias CellInfo =
    { clues : CellClues
    , start : ClueStart
    , separators : List CellSeparator
    }



-- Only editable cells exist in the grid. Absence = black cell.


type alias Grid =
    Dict ( Int, Int ) CellValue



type alias Selection =
    { clueId : ClueId
    , cellIndex : Int
    }



type alias Puzzle =
    { id : String
    , puzzleNumber : Int
    , name : String
    , setter : Maybe String
    , dimensions : { cols : Int, rows : Int }
    , crosswordType : String
    , clues : List Clue
    , cellInfos : Dict ( Int, Int ) CellInfo
    }



-- Model is a sum type: either we have a valid puzzle or a decode error.
-- No invalid state where both or neither exist.


type Model
    = Failed String
    | Active ActiveModel


type alias ActiveModel =
    { puzzle : Puzzle
    , grid : Grid
    , selection : Maybe Selection
    , navigationStyle : NavigationStyle
    }



-- Flat, descriptive messages.


type Msg
    = CellClicked Position
    | KeyPressed String Bool
    | ClueClicked ClueId
    | SetNavigation NavigationStyle
