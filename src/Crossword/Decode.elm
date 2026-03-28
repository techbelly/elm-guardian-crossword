module Crossword.Decode exposing (decodePuzzle, decodeGrid)

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
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


decodePuzzle : Decoder Puzzle
decodePuzzle =
    Decode.field "entries" (Decode.list decodeClue)
        |> Decode.andThen
            (\entries ->
                Decode.map5
                    (\id number name setter dims ->
                        buildPuzzle id number name setter dims entries
                    )
                    (Decode.field "id" Decode.string)
                    (Decode.field "number" Decode.int)
                    (Decode.field "name" Decode.string)
                    (Decode.maybe (Decode.at [ "creator", "name" ] Decode.string))
                    (Decode.field "dimensions" decodeDimensions)
                    |> Decode.andThen
                        (\buildFn ->
                            Decode.map buildFn
                                (Decode.field "crosswordType" Decode.string)
                        )
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
buildPuzzle id number name setter dims entries crosswordType =
    let
        acrossEntries =
            entries
                |> List.filter (\e -> e.id.direction == Across)
                |> List.sortBy (\e -> e.id.number)

        downEntries =
            entries
                |> List.filter (\e -> e.id.direction == Down)
                |> List.sortBy (\e -> e.id.number)

        sortedEntries =
            acrossEntries ++ downEntries

        cellInfos =
            buildCellInfos entries
    in
    { id = id
    , puzzleNumber = number
    , name = name
    , setter = setter
    , dimensions = dims
    , crosswordType = crosswordType
    , clues = sortedEntries
    , cellInfos = cellInfos
    }



-- Pre-compute CellInfo for every editable cell by iterating all entries.


buildCellInfos : List Clue -> Dict ( Int, Int ) CellInfo
buildCellInfos entries =
    List.foldl addClueToCellInfos Dict.empty entries


addClueToCellInfos : Clue -> Dict ( Int, Int ) CellInfo -> Dict ( Int, Int ) CellInfo
addClueToCellInfos entry cellInfos =
    let
        ( startCol, startRow ) =
            entry.position

        separatorPositions =
            entry.separators
                |> List.map
                    (\sep ->
                        case sep of
                            WordBoundary n ->
                                ( n, Bar )

                            Hyphen n ->
                                ( n, Dash )
                    )
    in
    List.range 0 (entry.length - 1)
        |> List.foldl
            (\idx acc ->
                let
                    cellCol =
                        case entry.id.direction of
                            Across ->
                                startCol + idx

                            Down ->
                                startCol

                    cellRow =
                        case entry.id.direction of
                            Across ->
                                startRow

                            Down ->
                                startRow + idx

                    key =
                        ( cellCol, cellRow )

                    isStart =
                        idx == 0

                    cellSeps =
                        separatorPositions
                            |> List.filterMap
                                (\( sepPos, sepKind ) ->
                                    if sepPos == idx then
                                        Just { direction = entry.id.direction, kind = sepKind }

                                    else
                                        Nothing
                                )

                    updated =
                        case Dict.get key acc of
                            Nothing ->
                                { clues =
                                    case entry.id.direction of
                                        Across ->
                                            AcrossOnly entry.id

                                        Down ->
                                            DownOnly entry.id
                                , start =
                                    if isStart then
                                        case entry.id.direction of
                                            Across ->
                                                StartsAcross entry.id.number

                                            Down ->
                                                StartsDown entry.id.number

                                    else
                                        NotStart
                                , separators = cellSeps
                                }

                            Just existing ->
                                { clues =
                                    case ( entry.id.direction, existing.clues ) of
                                        ( Across, DownOnly d ) ->
                                            AcrossAndDown { across = entry.id, down = d }

                                        ( Down, AcrossOnly a ) ->
                                            AcrossAndDown { across = a, down = entry.id }

                                        _ ->
                                            existing.clues
                                , start =
                                    if isStart then
                                        case ( entry.id.direction, existing.start ) of
                                            ( Across, StartsDown _ ) ->
                                                StartsBoth entry.id.number

                                            ( Down, StartsAcross _ ) ->
                                                StartsBoth entry.id.number

                                            ( Across, _ ) ->
                                                StartsAcross entry.id.number

                                            ( Down, _ ) ->
                                                StartsDown entry.id.number

                                    else
                                        existing.start
                                , separators = existing.separators ++ cellSeps
                                }
                in
                Dict.insert key updated acc
            )
            cellInfos



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
