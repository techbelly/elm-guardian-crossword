module Crossword.Decode exposing (buildCellInfos, decodePuzzle, decodeGrid)

import Crossword.Types as Types
    exposing
        ( CellClues(..)
        , ClueStart(..)
        , CellInfo
        , CellValue(..)
        , Direction(..)
        , Clue
        , ClueId
        , Grid
        , Puzzle
        , ClueSeparator(..)
        , SeparatorKind(..)
        )
import Crossword.Grid as Grid
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


decodePuzzle : Decoder Puzzle
decodePuzzle =
    Decode.map7 buildPuzzle
        (Decode.field "id" Decode.string)
        (Decode.field "number" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.maybe (Decode.at [ "creator", "name" ] Decode.string))
        (Decode.field "dimensions" decodeDimensions)
        (Decode.field "entries" (Decode.list decodeClue))
        (Decode.field "crosswordType" Decode.string)
        |> Decode.andThen validateGroups


validateGroups : Puzzle -> Decoder Puzzle
validateGroups puzzle =
    let
        firstInvalid =
            puzzle.clues
                |> List.concatMap .group
                |> List.filter (\cid -> Types.lookupClue cid puzzle == Nothing)
                |> List.head
    in
    case firstInvalid of
        Nothing ->
            Decode.succeed puzzle

        Just cid ->
            let
                dirStr =
                    case cid.direction of
                        Across ->
                            "across"

                        Down ->
                            "down"
            in
            Decode.fail
                ("Clue group references unknown clue: "
                    ++ String.fromInt cid.number
                    ++ "-"
                    ++ dirStr
                )


decodeDimensions : Decoder { cols : Int, rows : Int }
decodeDimensions =
    Decode.map2 (\c r -> { cols = c, rows = r })
        (Decode.field "cols" Decode.int)
        (Decode.field "rows" Decode.int)



decodeClueId : Decoder ClueId
decodeClueId =
    Decode.string
        |> Decode.andThen
            (\s ->
                case String.split "-" s of
                    [ numStr, dirStr ] ->
                        case ( String.toInt numStr, dirStr ) of
                            ( Just n, "across" ) ->
                                Decode.succeed { number = n, direction = Across }

                            ( Just n, "down" ) ->
                                Decode.succeed { number = n, direction = Down }

                            _ ->
                                Decode.fail ("Invalid entry ID: " ++ s)

                    _ ->
                        Decode.fail ("Invalid entry ID format: " ++ s)
            )


decodePosition : Decoder Types.Position
decodePosition =
    Decode.map2 Tuple.pair
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


decodeClueSeparatorLocations : Decoder (List ClueSeparator)
decodeClueSeparatorLocations =
    Decode.map2
        (\commas hyphens ->
            List.map WordBoundary commas ++ List.map Hyphen hyphens
        )
        (optionalField "," (Decode.list Decode.int) [])
        (optionalField "-" (Decode.list Decode.int) [])


optionalField : String -> Decoder a -> a -> Decoder a
optionalField fieldName decoder default =
    Decode.oneOf
        [ Decode.field fieldName decoder
        , Decode.succeed default
        ]


decodeClue : Decoder Clue
decodeClue =
    Decode.map8 Clue
        (Decode.field "id" decodeClueId)
        (Decode.field "humanNumber" Decode.string)
        (Decode.field "clue" Decode.string)
        (Decode.field "length" Decode.int)
        (Decode.field "position" decodePosition)
        (Decode.field "separatorLocations" decodeClueSeparatorLocations)
        (Decode.field "group" (Decode.list decodeClueId))
        (optionalField "solution" (Decode.map Just Decode.string) Nothing)



-- Build the full Puzzle from parsed entries, pre-computing all derived data.


buildPuzzle : String -> Int -> String -> Maybe String -> { cols : Int, rows : Int } -> List Clue -> String -> Puzzle
buildPuzzle id number name setter dims clues crosswordType =
    let
        acrossClues =
            clues
                |> List.filter (\clue -> clue.id.direction == Across)
                |> List.sortBy (\clue -> clue.id.number)

        downClues =
            clues
                |> List.filter (\clue -> clue.id.direction == Down)
                |> List.sortBy (\clue -> clue.id.number)

        sortedClues =
            acrossClues ++ downClues

        cellInfos =
            buildCellInfos clues
    in
    { id = id
    , puzzleNumber = number
    , name = name
    , setter = setter
    , dimensions = dims
    , crosswordType = crosswordType
    , clues = sortedClues
    , cellInfos = cellInfos
    }



-- Pre-compute CellInfo for every editable cell by iterating all entries.


buildCellInfos : List Clue -> Dict ( Int, Int ) CellInfo
buildCellInfos clues =
    List.foldl addClueToCellInfos Dict.empty clues


addClueToCellInfos : Clue -> Dict ( Int, Int ) CellInfo -> Dict ( Int, Int ) CellInfo
addClueToCellInfos clue cellInfos =
    let
        separatorPositions =
            clue.separators
                |> List.map
                    (\sep ->
                        case sep of
                            WordBoundary n ->
                                ( n, Bar )

                            Hyphen n ->
                                ( n, Dash )
                    )
    in
    List.range 0 (clue.length - 1)
        |> List.foldl
            (\idx acc ->
                let
                    key =
                        Grid.positionFromCellIndex idx clue

                    cellSeps =
                        separatorPositions
                            |> List.filterMap
                                (\( sepPos, sepKind ) ->
                                    if sepPos == idx then
                                        Just { direction = clue.id.direction, kind = sepKind }

                                    else
                                        Nothing
                                )

                    updated =
                        case Dict.get key acc of
                            Nothing ->
                                freshCellInfo clue.id (idx == 0) cellSeps

                            Just existing ->
                                mergeCellInfo clue.id (idx == 0) cellSeps existing
                in
                Dict.insert key updated acc
            )
            cellInfos


freshCellInfo : Types.ClueId -> Bool -> List Types.CellSeparator -> CellInfo
freshCellInfo clueId isStart cellSeps =
    { clues =
        case clueId.direction of
            Across ->
                AcrossOnly clueId

            Down ->
                DownOnly clueId
    , start =
        if isStart then
            case clueId.direction of
                Across ->
                    StartsAcross clueId.number

                Down ->
                    StartsDown clueId.number

        else
            NotStart
    , separators = cellSeps
    }


mergeCellInfo : Types.ClueId -> Bool -> List Types.CellSeparator -> CellInfo -> CellInfo
mergeCellInfo clueId isStart cellSeps existing =
    { clues =
        case ( clueId.direction, existing.clues ) of
            ( Across, DownOnly d ) ->
                AcrossAndDown { across = clueId, down = d }

            ( Down, AcrossOnly a ) ->
                AcrossAndDown { across = a, down = clueId }

            _ ->
                existing.clues
    , start =
        if isStart then
            case ( clueId.direction, existing.start ) of
                ( Across, StartsDown _ ) ->
                    StartsBoth clueId.number

                ( Down, StartsAcross _ ) ->
                    StartsBoth clueId.number

                ( Across, _ ) ->
                    StartsAcross clueId.number

                ( Down, _ ) ->
                    StartsDown clueId.number

        else
            existing.start
    , separators = existing.separators ++ cellSeps
    }



-- Decode saved grid from localStorage.


decodeGrid : Decoder Grid
decodeGrid =
    Decode.keyValuePairs decodeCellValue
        |> Decode.map
            (\pairs ->
                pairs
                    |> List.filterMap
                        (\( key, val ) ->
                            case String.split "," key of
                                [ colStr, rowStr ] ->
                                    case ( String.toInt colStr, String.toInt rowStr ) of
                                        ( Just c, Just r ) ->
                                            Just ( ( c, r ), val )

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing
                        )
                    |> Dict.fromList
            )


decodeCellValue : Decoder CellValue
decodeCellValue =
    Decode.string
        |> Decode.map
            (\s ->
                case String.uncons s of
                    Just ( ch, "" ) ->
                        Filled ch

                    _ ->
                        Empty
            )
