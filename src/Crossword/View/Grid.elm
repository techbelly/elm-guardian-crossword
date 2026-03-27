module Crossword.View.Grid exposing (viewGrid)

import Crossword.Grid as Grid
import Crossword.Types as Types
    exposing
        ( CellInfo
        , CellSeparator
        , CellValue(..)
        , ClueStart(..)
        , Direction(..)
        , Grid
        , Msg(..)
        , Puzzle
        , Selection
        , SeparatorKind(..)
        )
import Dict
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE


cellSize : Int
cellSize =
    31


borderSize : Int
borderSize =
    1


cellStep : Int
cellStep =
    cellSize + borderSize


numberFontSize : Int
numberFontSize =
    9


backgroundColor : String
backgroundColor =
    "#0a0a2e"


focusedColor : String
focusedColor =
    "#fff7b2"


highlightedColor : String
highlightedColor =
    "#d3e4fc"


viewGrid : Puzzle -> Grid -> Maybe Selection -> Html Msg
viewGrid puzzle grid selection =
    let
        width =
            puzzle.dimensions.cols * cellStep + borderSize

        height =
            puzzle.dimensions.rows * cellStep + borderSize

        highlighted =
            case selection of
                Just sel ->
                    highlightedPositions puzzle sel

                Nothing ->
                    Set.empty

        focusedPos =
            selection
                |> Maybe.andThen
                    (\sel ->
                        lookupClue sel.clueId puzzle
                            |> Maybe.map (Grid.clueCell sel.cellIndex)
                    )

        cells =
            puzzle.cellInfos
                |> Dict.toList
                |> List.concatMap
                    (\( ( c, r ), cellInfo ) ->
                        viewCell c r cellInfo grid focusedPos highlighted
                    )
    in
    Svg.svg
        [ SvgA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        , SvgA.width (String.fromInt width)
        , SvgA.height (String.fromInt height)
        ]
        (background width height :: cells)


background : Int -> Int -> Svg msg
background width height =
    Svg.rect
        [ SvgA.width (String.fromInt width)
        , SvgA.height (String.fromInt height)
        , SvgA.fill backgroundColor
        ]
        []


viewCell : Int -> Int -> CellInfo -> Grid -> Maybe ( Int, Int ) -> Set ( Int, Int ) -> List (Svg Msg)
viewCell c r cellInfo grid focusedPos highlighted =
    let
        left =
            c * cellStep + borderSize

        top =
            r * cellStep + borderSize

        pos =
            ( c, r )

        isFocused =
            focusedPos == Just ( c, r )

        isHighlighted =
            Set.member ( c, r ) highlighted

        fillColor =
            if isFocused then
                focusedColor

            else if isHighlighted then
                highlightedColor

            else
                "#ffffff"

        cellRect =
            Svg.rect
                [ SvgA.x (String.fromInt left)
                , SvgA.y (String.fromInt top)
                , SvgA.width (String.fromInt cellSize)
                , SvgA.height (String.fromInt cellSize)
                , SvgA.fill fillColor
                , SvgE.onClick (CellClicked pos)
                ]
                []

        numberText =
            case clueNumber cellInfo.start of
                Just n ->
                    [ Svg.text_
                        [ SvgA.x (String.fromInt (left + 1))
                        , SvgA.y (String.fromInt (top + numberFontSize))
                        , SvgA.fontSize (String.fromInt numberFontSize)
                        , SvgA.fill backgroundColor
                        , SvgA.style "pointer-events: none; user-select: none;"
                        ]
                        [ Svg.text (String.fromInt n) ]
                    ]

                Nothing ->
                    []

        letterText =
            case Grid.get pos grid of
                Filled ch ->
                    [ Svg.text_
                        [ SvgA.x (String.fromInt (left + cellSize // 2))
                        , SvgA.y (String.fromFloat (toFloat top + toFloat cellSize * 0.675))
                        , SvgA.fontSize "16"
                        , SvgA.textAnchor "middle"
                        , SvgA.fontWeight "bold"
                        , SvgA.fill backgroundColor
                        , SvgA.style "pointer-events: none; user-select: none;"
                        ]
                        [ Svg.text (String.fromChar ch) ]
                    ]

                Empty ->
                    []

        separators =
            List.concatMap (viewSeparator c r) cellInfo.separators
    in
    cellRect :: numberText ++ letterText ++ separators


viewSeparator : Int -> Int -> CellSeparator -> List (Svg msg)
viewSeparator c r sep =
    let
        left =
            c * cellStep + borderSize

        top =
            r * cellStep + borderSize
    in
    case ( sep.direction, sep.kind ) of
        ( Across, Bar ) ->
            -- Word boundary: vertical bar at left edge of cell
            [ Svg.rect
                [ SvgA.x (String.fromInt (left - borderSize))
                , SvgA.y (String.fromInt top)
                , SvgA.width (String.fromInt borderSize)
                , SvgA.height (String.fromInt cellSize)
                , SvgA.fill backgroundColor
                ]
                []
            , Svg.rect
                [ SvgA.x (String.fromInt (left - borderSize - 1))
                , SvgA.y (String.fromInt top)
                , SvgA.width "3"
                , SvgA.height (String.fromInt cellSize)
                , SvgA.fill backgroundColor
                ]
                []
            ]

        ( Down, Bar ) ->
            -- Word boundary: horizontal bar at top edge of cell
            [ Svg.rect
                [ SvgA.x (String.fromInt left)
                , SvgA.y (String.fromInt (top - borderSize - 1))
                , SvgA.width (String.fromInt cellSize)
                , SvgA.height "3"
                , SvgA.fill backgroundColor
                ]
                []
            ]

        ( Across, Dash ) ->
            -- Hyphen: short horizontal dash centered on left border
            let
                dashLen =
                    cellSize // 4

                dashY =
                    top + cellSize // 2
            in
            [ Svg.line
                [ SvgA.x1 (String.fromInt (left - borderSize - dashLen // 2))
                , SvgA.y1 (String.fromInt dashY)
                , SvgA.x2 (String.fromInt (left - borderSize + dashLen // 2))
                , SvgA.y2 (String.fromInt dashY)
                , SvgA.stroke backgroundColor
                , SvgA.strokeWidth "1"
                ]
                []
            ]

        ( Down, Dash ) ->
            -- Hyphen: short vertical dash centered on top border
            let
                dashLen =
                    cellSize // 4

                dashX =
                    left + cellSize // 2
            in
            [ Svg.line
                [ SvgA.x1 (String.fromInt dashX)
                , SvgA.y1 (String.fromInt (top - borderSize - dashLen // 2))
                , SvgA.x2 (String.fromInt dashX)
                , SvgA.y2 (String.fromInt (top - borderSize + dashLen // 2))
                , SvgA.stroke backgroundColor
                , SvgA.strokeWidth "1"
                ]
                []
            ]


highlightedPositions : Puzzle -> Selection -> Set ( Int, Int )
highlightedPositions puzzle sel =
    case lookupClue sel.clueId puzzle of
        Nothing ->
            Set.empty

        Just entry ->
            entry.group
                |> List.concatMap
                    (\eid ->
                        case lookupClue eid puzzle of
                            Just groupClue ->
                                List.range 0 (groupClue.length - 1)
                                    |> List.map
                                        (\idx ->
                                            Grid.clueCell idx groupClue
                                        )

                            Nothing ->
                                []
                    )
                |> Set.fromList


clueNumber : ClueStart -> Maybe Int
clueNumber start =
    case start of
        NotStart ->
            Nothing

        StartsAcross n ->
            Just n

        StartsDown n ->
            Just n

        StartsBoth n ->
            Just n


lookupClue : Types.ClueId -> Puzzle -> Maybe Types.Clue
lookupClue cid puzzle =
    puzzle.clues
        |> List.filter (\e -> e.id == cid)
        |> List.head
