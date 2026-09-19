module HistoryTest exposing (suite)

import Crossword.History as History exposing (Entry)
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Crossword.History"
        [ describe "completion"
            [ test "a full grid is complete" <|
                \_ -> History.completed (entry "cryptic" 1 |> progress 68 68) |> Expect.equal True
            , test "one empty cell is not" <|
                \_ -> History.completed (entry "cryptic" 1 |> progress 67 68) |> Expect.equal False
            , test "an empty puzzle with no cells is not complete" <|
                \_ -> History.completed (entry "cryptic" 1 |> progress 0 0) |> Expect.equal False
            ]
        , describe "grouping"
            [ test "groups by series, highest number first within a series" <|
                \_ ->
                    [ entry "cryptic" 100, entry "prize" 50, entry "cryptic" 102 ]
                        |> History.grouped
                        |> List.map (\( series, group ) -> ( series, List.map .number group ))
                        |> Expect.equal [ ( "cryptic", [ 102, 100 ] ), ( "prize", [ 50 ] ) ]
            , test "the most recently opened series comes first" <|
                \_ ->
                    [ entry "cryptic" 100 |> opened 1000, entry "prize" 50 |> opened 9000 ]
                        |> History.grouped
                        |> List.map Tuple.first
                        |> Expect.equal [ "prize", "cryptic" ]
            ]
        , describe "merge"
            [ test "replaces the entry for a path already held" <|
                \_ ->
                    [ entry "cryptic" 100 |> progress 10 68, entry "cryptic" 101 ]
                        |> History.merge (entry "cryptic" 100 |> progress 68 68)
                        |> List.filter (\e -> e.number == 100)
                        |> List.map .filled
                        |> Expect.equal [ 68 ]
            , test "adds an entry for a path not held" <|
                \_ ->
                    [ entry "cryptic" 100 ]
                        |> History.merge (entry "prize" 7)
                        |> List.length
                        |> Expect.equal 2
            ]
        , describe "tallies"
            [ test "counts solved against started per series" <|
                \_ ->
                    [ entry "cryptic" 1 |> progress 68 68
                    , entry "cryptic" 2 |> progress 10 68
                    , entry "prize" 3 |> progress 68 68
                    ]
                        |> History.seriesTallies
                        |> List.map (\( series, t ) -> ( series, t.completed, t.started ))
                        |> Expect.equal [ ( "cryptic", 1, 2 ), ( "prize", 1, 1 ) ]
            , test "averages elapsed over solved puzzles only" <|
                \_ ->
                    [ entry "cryptic" 1 |> progress 68 68 |> took 60000
                    , entry "cryptic" 2 |> progress 10 68 |> took 999999
                    ]
                        |> History.seriesTallies
                        |> List.map (\( _, t ) -> t.totalElapsed)
                        |> Expect.equal [ 60000 ]
            , test "buckets by the weekday the puzzle was published" <|
                \_ ->
                    -- 1774396800000 is Wednesday 25 March 2026, midnight UTC
                    [ entry "cryptic" 1 |> publishedAt 1774396800000 |> progress 68 68 ]
                        |> History.weekdayTallies
                        |> List.filter (\( _, t ) -> t.started > 0)
                        |> List.map (\( day, t ) -> ( day, t.completed ))
                        |> Expect.equal [ ( Time.Wed, 1 ) ]
            ]
        , describe "storage"
            [ test "an entry survives a round trip" <|
                \_ ->
                    let
                        original =
                            entry "cryptic" 29963 |> progress 30 68 |> took 125000
                    in
                    Json.Encode.list History.encode [ original ]
                        |> Json.Decode.decodeValue History.decoder
                        |> Expect.equal (Ok [ original ])
            , test "an unreadable record is skipped rather than losing the list" <|
                \_ ->
                    Json.Encode.list identity
                        [ Json.Encode.string "nonsense", History.encode (entry "cryptic" 1) ]
                        |> Json.Decode.decodeValue History.decoder
                        |> Result.map (List.map .number)
                        |> Expect.equal (Ok [ 1 ])
            ]
        ]



-- FIXTURES


entry : String -> Int -> Entry
entry series number =
    { path = series ++ "/" ++ String.fromInt number
    , series = series
    , number = number
    , name = "Crossword No " ++ String.fromInt number
    , setter = Just "Qaos"
    , published = Time.millisToPosix 0
    , lastOpened = Time.millisToPosix 0
    , elapsed = 0
    , filled = 0
    , cells = 68
    }


progress : Int -> Int -> Entry -> Entry
progress filled cells e =
    { e | filled = filled, cells = cells }


took : Int -> Entry -> Entry
took elapsed e =
    { e | elapsed = elapsed }


opened : Int -> Entry -> Entry
opened millis e =
    { e | lastOpened = Time.millisToPosix millis }


publishedAt : Int -> Entry -> Entry
publishedAt millis e =
    { e | published = Time.millisToPosix millis }
