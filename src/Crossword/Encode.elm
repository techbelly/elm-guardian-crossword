module Crossword.Encode exposing (encodeGrid)

import Crossword.Types exposing (CellValue(..), Grid)
import Dict
import Json.Encode as Encode


{-| The payload saved to localStorage: the cells entered so far, tagged with
the puzzle they belong to so the JS side knows where to put them.
-}
encodeGrid : String -> Grid -> Encode.Value
encodeGrid puzzleId grid =
    Encode.object
        [ ( "puzzleId", Encode.string puzzleId )
        , ( "cells", encodeCells grid )
        ]


encodeCells : Grid -> Encode.Value
encodeCells grid =
    grid
        |> Dict.toList
        |> List.filterMap
            (\( ( c, r ), val ) ->
                case val of
                    Filled ch ->
                        Just
                            ( String.fromInt c ++ "," ++ String.fromInt r
                            , Encode.string (String.fromChar ch)
                            )

                    Empty ->
                        Nothing
            )
        |> Encode.object
