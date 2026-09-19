module Anagram.Enumeration exposing
    ( Enumeration
    , alternatives
    , fromClue
    , parse
    , toText
    , totalLetters
    )

{-| The bracketed letter counts at the end of a cryptic clue — `(8)`, `(4,7)`,
`(1-2,8)`.

A hyphenated run is ambiguous for anagram purposes: `(3-4)` may be held in the
dictionary as one seven-letter entry ("cross-eyed" style) or as two separate
words, and there is no way to know which without looking. Rather than guess,
each hyphenated run contributes two alternatives — fully split or fully joined —
and `alternatives` returns every combination across the runs. A search satisfies
the enumeration if it matches any one of them.

-}


type Enumeration
    = Enumeration Part (List Part)


{-| A comma-separated element of the enumeration. `Hyphenated` carries its first
two components separately so a hyphenated run of fewer than two can't be built.
-}
type Part
    = Word Int
    | Hyphenated Int Int (List Int)


parts : Enumeration -> List Part
parts (Enumeration first rest) =
    first :: rest



-- PARSING


{-| Parse the enumeration from the end of a full clue, e.g.
"Confused starlet picked her own jewel (8)" → `(8)`. Takes the last
parenthesised group so a clue containing earlier brackets still works.
-}
fromClue : String -> Maybe Enumeration
fromClue clue =
    case lastBracketed clue of
        Nothing ->
            Nothing

        Just inner ->
            parse inner


{-| Parse bare enumeration text such as "4,7" or "1-2,8". Surrounding brackets
are tolerated so the modal's editable field accepts either form.
-}
parse : String -> Maybe Enumeration
parse text =
    let
        stripped =
            text
                |> String.trim
                |> stripWrapping
                |> String.trim
    in
    if String.isEmpty stripped then
        Nothing

    else
        stripped
            |> String.split ","
            |> List.map parsePart
            |> allJust
            |> Maybe.andThen toEnumeration


toEnumeration : List Part -> Maybe Enumeration
toEnumeration list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            Just (Enumeration first rest)


parsePart : String -> Maybe Part
parsePart text =
    case text |> String.split "-" |> List.map parseNumber |> allJust of
        Just [ n ] ->
            Just (Word n)

        Just (a :: b :: rest) ->
            Just (Hyphenated a b rest)

        _ ->
            Nothing


parseNumber : String -> Maybe Int
parseNumber text =
    case String.toInt (String.trim text) of
        Just n ->
            if n > 0 then
                Just n

            else
                Nothing

        Nothing ->
            Nothing


{-| The contents of the last `(...)` group, if the string ends with one.
-}
lastBracketed : String -> Maybe String
lastBracketed text =
    let
        trimmed =
            String.trim text
    in
    if String.endsWith ")" trimmed then
        let
            body =
                String.dropRight 1 trimmed
        in
        body
            |> String.indexes "("
            |> List.reverse
            |> List.head
            |> Maybe.map (\i -> String.dropLeft (i + 1) body)

    else
        Nothing


stripWrapping : String -> String
stripWrapping text =
    if String.startsWith "(" text && String.endsWith ")" text then
        text |> String.dropLeft 1 |> String.dropRight 1

    else
        text



-- INTERPRETATION


{-| Every word-length combination the enumeration allows, one per way of
resolving its hyphenated runs. Lengths within a combination are unordered — the
search treats them as a multiset.
-}
alternatives : Enumeration -> List (List Int)
alternatives enumeration =
    enumeration
        |> parts
        |> List.foldr extend [ [] ]


extend : Part -> List (List Int) -> List (List Int)
extend part tails =
    partOptions part
        |> List.concatMap (\option -> List.map (\tail -> option ++ tail) tails)


partOptions : Part -> List (List Int)
partOptions part =
    case part of
        Word n ->
            [ [ n ] ]

        Hyphenated a b rest ->
            let
                split =
                    a :: b :: rest
            in
            [ split, [ List.sum split ] ]


{-| Total letters the enumeration accounts for — the same whichever alternative
is taken.
-}
totalLetters : Enumeration -> Int
totalLetters enumeration =
    enumeration
        |> parts
        |> List.map partLetters
        |> List.sum


partLetters : Part -> Int
partLetters part =
    case part of
        Word n ->
            n

        Hyphenated a b rest ->
            a + b + List.sum rest


{-| Canonical text form, e.g. "1-2,8". Used to seed the modal's editable field.
-}
toText : Enumeration -> String
toText enumeration =
    enumeration
        |> parts
        |> List.map partToText
        |> String.join ","


partToText : Part -> String
partToText part =
    case part of
        Word n ->
            String.fromInt n

        Hyphenated a b rest ->
            (a :: b :: rest)
                |> List.map String.fromInt
                |> String.join "-"



-- HELPERS


allJust : List (Maybe a) -> Maybe (List a)
allJust list =
    List.foldr
        (\item acc -> Maybe.map2 (::) item acc)
        (Just [])
        list
