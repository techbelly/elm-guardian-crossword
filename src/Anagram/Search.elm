module Anagram.Search exposing
    ( Config
    , Result
    , defaults
    , sanitise
    , search
    , sortedKey
    )

import Anagram.Dict as Dict exposing (Dictionary)



{-| A single result is the list of dictionary entries that together use every
input letter exactly once. The list is in the order picked by the search.
-}
type alias Result =
    List String


type alias Config =
    { maxWords : Int
    , minWordLength : Int
    , maxResults : Int
    }


defaults : Config
defaults =
    { maxWords = 4
    , minWordLength = 3
    , maxResults = 30
    }



-- INPUT PREPARATION


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



-- SEARCH


{-| Find anagrams of the sanitised input. All input letters must be used.
Results are sorted by longest individual word descending, then alphabetically,
then capped at config.maxResults.
-}
search : Config -> Dictionary -> String -> List Result
search config dict input =
    let
        target =
            sortedKey input
    in
    if target == "" then
        []

    else
        let
            candidates =
                Dict.keys dict
                    |> List.filter (\k -> String.length k >= config.minWordLength)
                    |> List.sort
        in
        searchHelp config dict candidates target []
            |> rank
            |> List.take config.maxResults


searchHelp : Config -> Dictionary -> List String -> String -> List String -> List Result
searchHelp config dict candidates remaining picked =
    if remaining == "" then
        expand dict (List.reverse picked)

    else if List.length picked >= config.maxWords then
        []

    else
        candidates
            |> List.concatMap
                (\k ->
                    if String.length k > String.length remaining then
                        []

                    else
                        case subtract remaining k of
                            Nothing ->
                                []

                            Just newRemaining ->
                                searchHelp config dict (dropBefore k candidates) newRemaining (k :: picked)
                )


{-| Drop list entries that come strictly before `pivot` (lex order). The pivot
itself is kept so the same key can be reused (e.g. anagram = same word twice).
-}
dropBefore : String -> List String -> List String
dropBefore pivot list =
    case list of
        [] ->
            []

        x :: rest ->
            if x < pivot then
                dropBefore pivot rest

            else
                list


{-| Multiset subtraction on sorted-letter strings. Returns Nothing if `sub` is
not a multiset-subset of `super`.
-}
subtract : String -> String -> Maybe String
subtract super sub =
    subtractHelp (String.toList super) (String.toList sub) []


subtractHelp : List Char -> List Char -> List Char -> Maybe String
subtractHelp super sub acc =
    case sub of
        [] ->
            Just (String.fromList (List.reverse acc ++ super))

        s :: subRest ->
            case super of
                [] ->
                    Nothing

                p :: pRest ->
                    if p == s then
                        subtractHelp pRest subRest acc

                    else if p < s then
                        subtractHelp pRest sub (p :: acc)

                    else
                        Nothing


{-| Turn a list of picked sorted-letter keys into all word-tuple combinations.
-}
expand : Dictionary -> List String -> List Result
expand dict picked =
    picked
        |> List.map (\k -> Dict.lookup k dict)
        |> cartesian


cartesian : List (List a) -> List (List a)
cartesian lists =
    case lists of
        [] ->
            [ [] ]

        firstList :: restLists ->
            let
                rest =
                    cartesian restLists
            in
            firstList
                |> List.concatMap (\x -> List.map (\r -> x :: r) rest)



-- RANKING


{-| Sort by longest individual word descending, then by joined string ascending.
-}
rank : List Result -> List Result
rank results =
    results
        |> List.sortBy rankKey


rankKey : Result -> ( Int, String )
rankKey r =
    ( -(longestWordLength r), String.join " " r )


longestWordLength : Result -> Int
longestWordLength r =
    r
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0
