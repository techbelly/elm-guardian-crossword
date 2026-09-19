module Crossword.History exposing
    ( Entry
    , Tally
    , completed
    , decoder
    , encode
    , grouped
    , merge
    , seriesTallies
    , weekdayTallies
    )

{-| The record of crosswords opened on this machine, kept in localStorage.

One entry per puzzle, holding enough to list it, re-fetch it, and report on it:
where it came from, how far through it is, and how long it has taken. Entries
are written as the puzzle is solved, so the list doubles as the set of puzzles
in progress.

-}

import Json.Decode as Decode exposing (Decoder)
import Set
import Json.Encode as Encode
import Time


type alias Entry =
    { path : String
    , series : String
    , number : Int
    , name : String
    , setter : Maybe String
    , published : Time.Posix
    , lastOpened : Time.Posix
    , elapsed : Int
    , filled : Int
    , cells : Int
    }


{-| A puzzle counts as completed once every cell holds a letter. Nothing here
checks those letters against the solution.
-}
completed : Entry -> Bool
completed entry =
    entry.cells > 0 && entry.filled >= entry.cells



{-| Replace the entry for this puzzle, or add it if it is new. Used when
returning to the list mid-solve so progress shows without a round trip through
storage.
-}
merge : Entry -> List Entry -> List Entry
merge entry entries =
    if List.any (\existing -> existing.path == entry.path) entries then
        List.map
            (\existing ->
                if existing.path == entry.path then
                    entry

                else
                    existing
            )
            entries

    else
        entry :: entries



-- ORDERING


{-| Entries grouped by series — cryptic, prize, quiptic — with the most
recently visited series first and the highest-numbered puzzle first within it.
-}
grouped : List Entry -> List ( String, List Entry )
grouped entries =
    entries
        |> bySeries
        |> List.sortBy (\( _, group ) -> negate (mostRecent group))
        |> List.map (\( series, group ) -> ( series, List.sortBy (\e -> negate e.number) group ))


mostRecent : List Entry -> Int
mostRecent group =
    group
        |> List.map (.lastOpened >> Time.posixToMillis)
        |> List.maximum
        |> Maybe.withDefault 0


bySeries : List Entry -> List ( String, List Entry )
bySeries entries =
    entries
        |> List.map .series
        |> distinct
        |> List.map (\series -> ( series, List.filter (\entry -> entry.series == series) entries ))


distinct : List String -> List String
distinct values =
    values
        |> List.foldl
            (\value ( seen, acc ) ->
                if Set.member value seen then
                    ( seen, acc )

                else
                    ( Set.insert value seen, value :: acc )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse



-- STATISTICS


type alias Tally =
    { completed : Int
    , started : Int
    , totalElapsed : Int
    }


tally : List Entry -> Tally
tally entries =
    { completed = entries |> List.filter completed |> List.length
    , started = List.length entries
    , totalElapsed =
        entries
            |> List.filter completed
            |> List.map .elapsed
            |> List.sum
    }


seriesTallies : List Entry -> List ( String, Tally )
seriesTallies entries =
    entries
        |> grouped
        |> List.map (\( series, group ) -> ( series, tally group ))


{-| Completion by the day of the week the puzzle was *published*, which is what
makes a Friday cryptic comparable with another Friday cryptic. Guardian dates
are midnight UTC on the publication day, so UTC is the right zone to read them
in.
-}
weekdayTallies : List Entry -> List ( Time.Weekday, Tally )
weekdayTallies entries =
    weekdays
        |> List.map
            (\day ->
                ( day
                , entries
                    |> List.filter (\entry -> Time.toWeekday Time.utc entry.published == day)
                    |> tally
                )
            )


weekdays : List Time.Weekday
weekdays =
    [ Time.Mon, Time.Tue, Time.Wed, Time.Thu, Time.Fri, Time.Sat, Time.Sun ]



-- STORAGE


encode : Entry -> Encode.Value
encode entry =
    Encode.object
        [ ( "path", Encode.string entry.path )
        , ( "series", Encode.string entry.series )
        , ( "number", Encode.int entry.number )
        , ( "name", Encode.string entry.name )
        , ( "setter", entry.setter |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "published", Encode.int (Time.posixToMillis entry.published) )
        , ( "lastOpened", Encode.int (Time.posixToMillis entry.lastOpened) )
        , ( "elapsed", Encode.int entry.elapsed )
        , ( "filled", Encode.int entry.filled )
        , ( "cells", Encode.int entry.cells )
        ]


decoder : Decoder (List Entry)
decoder =
    Decode.list entryDecoder
        |> Decode.map (List.filterMap identity)


{-| An entry that can't be read is skipped rather than failing the whole list,
so one bad record can't cost you the history.
-}
entryDecoder : Decoder (Maybe Entry)
entryDecoder =
    Decode.oneOf
        [ Decode.map Just strictEntryDecoder
        , Decode.succeed Nothing
        ]


strictEntryDecoder : Decoder Entry
strictEntryDecoder =
    Decode.map8
        (\path series number name setter published lastOpened rest ->
            { path = path
            , series = series
            , number = number
            , name = name
            , setter = setter
            , published = published
            , lastOpened = lastOpened
            , elapsed = rest.elapsed
            , filled = rest.filled
            , cells = rest.cells
            }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "series" Decode.string)
        (Decode.field "number" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "setter" (Decode.nullable Decode.string))
        (Decode.field "published" posixDecoder)
        (Decode.field "lastOpened" posixDecoder)
        (Decode.map3 (\elapsed filled cells -> { elapsed = elapsed, filled = filled, cells = cells })
            (Decode.field "elapsed" Decode.int)
            (Decode.field "filled" Decode.int)
            (Decode.field "cells" Decode.int)
        )


posixDecoder : Decoder Time.Posix
posixDecoder =
    Decode.map Time.millisToPosix Decode.int
