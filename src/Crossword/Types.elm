module Crossword.Types exposing
    ( ActiveModel
    , LandingModel
    , LoadState(..)
    , AnagramModalState(..)
    , AnagramModalData
    , AnagramSearchOutcome(..)
    , CellClues(..)
    , ClueStart(..)
    , CellInfo
    , CellSeparator
    , CellValue(..)
    , DictionaryState(..)
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

import Anagram.Dict exposing (Dictionary)
import Anagram.Fodder exposing (Token)
import Browser.Events
import Crossword.History as History
import Crossword.Timer exposing (Timer)
import Dict exposing (Dict)
import Json.Decode
import Time


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


clueIdForDirection : Direction -> CellInfo -> Maybe ClueId
clueIdForDirection dir info =
    case ( dir, info.clues ) of
        ( Across, AcrossOnly cid ) ->
            Just cid

        ( Across, AcrossAndDown both ) ->
            Just both.across

        ( Down, DownOnly cid ) ->
            Just cid

        ( Down, AcrossAndDown both ) ->
            Just both.down

        _ ->
            Nothing


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
    , published : Time.Posix
    , puzzleNumber : Int
    , name : String
    , setter : Maybe String
    , dimensions : { cols : Int, rows : Int }
    , crosswordType : String
    , clues : List Clue
    , cellInfos : Dict ( Int, Int ) CellInfo
    }



-- Model is a sum type: either we are choosing a crossword or solving one.
-- No invalid state where both or neither exist.


type Model
    = Landing LandingModel
    | Active ActiveModel


type alias LandingModel =
    { path : String
    , history : List History.Entry
    , load : LoadState
    , now : Time.Posix
    }


{-| Fetching a puzzle is the one thing the landing page does; its outcome is
either still pending or a reason it didn't work.
-}
type LoadState
    = NotLoading
    | Loading
    | LoadFailed String


type alias ActiveModel =
    { puzzle : Puzzle
    , path : String
    , grid : Grid
    , selection : Maybe Selection
    , navigationStyle : NavigationStyle
    , clueSelection : String
    , dictionary : DictionaryState
    , anagramModal : AnagramModalState
    , timer : Timer
    , now : Time.Posix
    , history : List History.Entry
    }



-- Dictionary load lifecycle. Loaded once, on first modal open, then cached.


type DictionaryState
    = DictNotLoaded
    | DictLoading
    | DictReady Dictionary
    | DictFailed String



-- Anagram modal state. Closed by default; opening snapshots the selected clue
-- as togglable fodder tokens plus its enumeration.


type AnagramModalState
    = AnagramClosed
    | AnagramOpen AnagramModalData


type alias AnagramModalData =
    { tokens : List Token
    , extra : String
    , enumeration : String
    , lastSearch : Maybe AnagramSearchOutcome
    }


type AnagramSearchOutcome
    = AnagramSearching
    | AnagramTooShort
    | AnagramTooLong
    | AnagramNoResults
    | AnagramNoResultsForLengths
    | AnagramResults (List (List String))



-- Flat, descriptive messages.


type Msg
    = PathChanged String
    | LoadRequested
    | HistoryEntryClicked String
    | PuzzleLoaded Json.Decode.Value
    | PuzzleLoadFailed String
    | Tick Time.Posix
    | VisibilityChanged Browser.Events.Visibility
    | BackToLanding
    | CellClicked Position
    | KeyPressed String Bool
    | TextEntered String
    | ClueClicked ClueId
    | SetNavigation NavigationStyle
    | ClueSelectionChanged String
    | OpenAnagramModal
    | CloseAnagramModal
    | AnagramTokenToggled Int
    | AnagramExtraChanged String
    | AnagramEnumerationChanged String
    | AnagramSubmit
    | AnagramRunSearch
    | DictionaryLoaded Json.Decode.Value
    | DictionaryLoadFailed String
    | FocusRestored
    | NoopClick
