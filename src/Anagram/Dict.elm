module Anagram.Dict exposing
    ( Dictionary
    , decoder
    , fromList
    , keys
    , lookup
    )

import Dict exposing (Dict)
import Json.Decode as D



{-| Sorted-letters key (lowercase, ascending) mapped to the dictionary entries
sharing those letters. Phrases ("NEW YORK") are indexed under their letters with
spaces stripped from the key, but the displayed value preserves spaces.
-}
type Dictionary
    = Dictionary (Dict String (List String))


decoder : D.Decoder Dictionary
decoder =
    D.map Dictionary (D.dict (D.list D.string))


keys : Dictionary -> List String
keys (Dictionary d) =
    Dict.keys d


lookup : String -> Dictionary -> List String
lookup k (Dictionary d) =
    Dict.get k d |> Maybe.withDefault []


{-| Build a Dictionary from an explicit key→entries list. Caller is responsible
for the keys being correctly sorted-letter form. Primarily used by tests; the
production path goes through `decoder`.
-}
fromList : List ( String, List String ) -> Dictionary
fromList entries =
    Dictionary (Dict.fromList entries)
