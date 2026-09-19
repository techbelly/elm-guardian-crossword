module Anagram.Dict exposing
    ( Dictionary
    , Entry
    , decoder
    , entriesOfLength
    , fromList
    , lookup
    )

{-| The anagram dictionary: sorted-letter keys mapped to the entries sharing
those letters. Phrases ("NEW YORK") are indexed under their letters with spaces
stripped from the key, while the displayed value preserves them.

Alongside the lookup table the dictionary keeps every key bucketed by length
and tagged with its `Mask`, computed once when the dictionary is decoded. Search
only ever walks these buckets; the lookup table is consulted at the end to turn
winning keys back into displayable words.

-}

import Anagram.Mask as Mask exposing (Mask)
import Dict exposing (Dict)
import Json.Decode as D


type Dictionary
    = Dictionary
        { byKey : Dict String (List String)
        , byLength : Dict Int (List Entry)
        }


{-| A dictionary key with its precomputed mask. `length` is carried rather than
recomputed because the length-constrained search consults it per candidate.
-}
type alias Entry =
    { key : String
    , length : Int
    , mask : Mask
    }


decoder : D.Decoder Dictionary
decoder =
    D.map build (D.dict (D.list D.string))


build : Dict String (List String) -> Dictionary
build byKey =
    Dictionary
        { byKey = byKey
        , byLength = Dict.foldl (\key _ acc -> indexByLength key acc) Dict.empty byKey
        }


indexByLength : String -> Dict Int (List Entry) -> Dict Int (List Entry)
indexByLength key acc =
    let
        entry =
            { key = key
            , length = String.length key
            , mask = Mask.fromSortedKey key
            }
    in
    Dict.update entry.length (\existing -> Just (entry :: Maybe.withDefault [] existing)) acc


lookup : String -> Dictionary -> List String
lookup k (Dictionary d) =
    Dict.get k d.byKey |> Maybe.withDefault []


{-| Every key of exactly this length. The search unions the buckets it needs
rather than filtering the whole dictionary.
-}
entriesOfLength : Int -> Dictionary -> List Entry
entriesOfLength n (Dictionary d) =
    Dict.get n d.byLength |> Maybe.withDefault []


{-| Build a Dictionary from an explicit key→entries list. Caller is responsible
for the keys being correctly sorted-letter form. Primarily used by tests; the
production path goes through `decoder`.
-}
fromList : List ( String, List String ) -> Dictionary
fromList entries =
    build (Dict.fromList entries)
