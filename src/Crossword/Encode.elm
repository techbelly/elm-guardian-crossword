module Crossword.Encode exposing (encodeGrid)

import Crossword.Types exposing (CellValue(..), Grid)
import Dict
import Json.Encode as Encode


encodeGrid : Grid -> Encode.Value
encodeGrid grid =
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
