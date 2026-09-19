module Anagram.Letters exposing
    ( sanitise
    , sortedKey
    )

{-| Reducing text to the letters an anagram is made of.

Both the dictionary and the search have to agree on this exactly — the
dictionary indexes its entries under `sortedKey`, and the search looks them up
by it — so it lives apart from either.

-}


{-| Strip everything but letters and lowercase. Used before validation and
search so the modal accepts pasted-in clue text containing punctuation, digits,
and the cryptic enumeration `(8)`.
-}
sanitise : String -> String
sanitise input =
    input
        |> String.toLower
        |> String.filter Char.isAlpha


{-| Sorted lowercase letters of a string — the multiset key.
For phrases ("NEW YORK") spaces are stripped along with all non-letters.
-}
sortedKey : String -> String
sortedKey s =
    s
        |> sanitise
        |> String.toList
        |> List.sort
        |> String.fromList
