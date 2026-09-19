module Anagram.Search exposing
    ( Config
    , Result
    , WordLengths(..)
    , defaults
    , maxResultsFor
    , search
    )

{-| Finding every way to spell the input letters out of dictionary entries.

The search runs in three stages:

1.  **Prune.** Only keys whose letters fit inside the target can appear in any
    solution. One pass over the length buckets, rejecting on `Mask` first and
    on exact multiset containment second, cuts ~217k keys to a few hundred.

2.  **Cover.** Combinations are built by repeatedly picking the *rarest*
    remaining letter and trying only candidates containing it. Every branch
    therefore consumes that letter, which bounds the depth and collapses the
    branching factor — the alternative, trying every candidate at every node,
    is what made this slow. Word counts are explored shallowest-first so the
    result cap truncates the least interesting tail.

3.  **Expand.** Winning keys are turned back into the words sharing them.

-}

import Anagram.Dict as Dict exposing (Dictionary, Entry)
import Anagram.Letters exposing (sortedKey)
import Anagram.Mask as Mask exposing (Mask)
import Dict as CoreDict exposing (Dict)
import Set exposing (Set)


{-| A single result is the list of dictionary entries that together use every
input letter exactly once.
-}
type alias Result =
    List String


{-| Whether the caller knows how the letters split across words. `OneOf` holds
the alternatives an enumeration allows; a combination satisfies it by matching
any one of them, in any order.
-}
type WordLengths
    = AnyLengths
    | OneOf (List (List Int))


type alias Config =
    { maxWords : Int
    , minWordLength : Int
    , maxResults : Int
    , lengths : WordLengths
    }


{-| Results are ordered alphabetically within a word-count tier, so the cap
doesn't trim the least likely answers — it trims the end of the alphabet. A free
search has far more results than anyone will read, so it caps low and stays
quick; callers that narrow the search with an enumeration should raise the cap,
because there the set is small enough to show whole and the answer must not be
the one cut. See `maxResultsFor`.
-}
defaults : Config
defaults =
    { maxWords = 4
    , minWordLength = 3
    , maxResults = 50
    , lengths = AnyLengths
    }


{-| How many results are worth finding for a given constraint. Searching for the
first fifty free-form results takes tens of milliseconds; searching for two
hundred takes seconds, because the cheap shallow combinations run out. An
enumeration narrows the candidates enough that the larger cap stays instant.
-}
maxResultsFor : WordLengths -> Int
maxResultsFor lengths =
    case lengths of
        AnyLengths ->
            50

        OneOf _ ->
            200



-- SEARCH


{-| Find anagrams of the sanitised input. All input letters must be used.
Results are sorted by longest individual word descending, then alphabetically,
then capped at `config.maxResults`.
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
                prune config dict target

            ctx =
                buildContext config.maxResults candidates
        in
        cover config ctx target
            |> List.concatMap (expand (admissible config.lengths) dict)
            |> rank
            |> List.take config.maxResults



-- STAGE 1: PRUNE


{-| Dictionary keys that could take part in a solution: right length, letters
available in the target.
-}
prune : Config -> Dictionary -> String -> List String
prune config dict target =
    let
        targetMask =
            Mask.fromSortedKey target
    in
    candidateLengths config (String.length target)
        |> List.concatMap (\n -> Dict.entriesOfLength n dict)
        |> List.filterMap (keepCandidate targetMask target)


keepCandidate : Mask -> String -> Entry -> Maybe String
keepCandidate targetMask target entry =
    if Mask.subsetOf entry.mask targetMask && subtract target entry.key /= Nothing then
        Just entry.key

    else
        Nothing


{-| The key lengths worth scanning. Under an enumeration only the lengths it
mentions can appear at all, which is the cheapest pruning available.
-}
candidateLengths : Config -> Int -> List Int
candidateLengths config targetLength =
    case config.lengths of
        AnyLengths ->
            List.range config.minWordLength targetLength

        OneOf alternatives ->
            alternatives
                |> List.concat
                |> List.filter (\n -> n <= targetLength)
                |> distinct



-- STAGE 2: COVER


type alias Context =
    { buckets : Dict Char (List String)
    , sizes : Dict Char Int
    , longest : Int
    , maxResults : Int
    }


buildContext : Int -> List String -> Context
buildContext maxResults candidates =
    let
        buckets =
            List.foldl indexByLetter CoreDict.empty candidates
    in
    { buckets = buckets
    , sizes = CoreDict.map (\_ keys -> List.length keys) buckets
    , longest =
        candidates
            |> List.map String.length
            |> List.maximum
            |> Maybe.withDefault 0
    , maxResults = maxResults
    }


indexByLetter : String -> Dict Char (List String) -> Dict Char (List String)
indexByLetter key buckets =
    key
        |> String.toList
        |> distinct
        |> List.foldl
            (\c acc -> CoreDict.update c (\existing -> Just (key :: Maybe.withDefault [] existing)) acc)
            buckets


type alias Accumulator =
    { combos : List (List String)
    , seen : Set String
    , count : Int
    }


emptyAccumulator : Accumulator
emptyAccumulator =
    { combos = [], seen = Set.empty, count = 0 }


{-| Key combinations covering the target exactly.
-}
cover : Config -> Context -> String -> List (List String)
cover config ctx target =
    let
        final =
            case config.lengths of
                AnyLengths ->
                    List.range 1 config.maxWords
                        |> List.foldl (deepenTo config ctx target) emptyAccumulator

                OneOf alternatives ->
                    alternatives
                        |> List.filter (\lengths -> List.sum lengths == String.length target)
                        |> List.foldl (coverLengths ctx target) emptyAccumulator
    in
    List.reverse final.combos


{-| One round of iterative deepening: combinations of exactly `wordCount` keys.
Shallower rounds have already run, so nothing is rediscovered.
-}
deepenTo : Config -> Context -> String -> Int -> Accumulator -> Accumulator
deepenTo config ctx target wordCount acc =
    coverFree config ctx wordCount target [] acc


coverFree : Config -> Context -> Int -> String -> List String -> Accumulator -> Accumulator
coverFree config ctx wordsLeft remaining picked acc =
    if isFull ctx acc then
        acc

    else if remaining == "" then
        if wordsLeft == 0 then
            record (List.sort picked) acc

        else
            acc

    else if not (coverable ctx config.minWordLength wordsLeft remaining) then
        acc

    else
        rarestBucket ctx remaining
            |> List.foldl
                (\key inner ->
                    case subtract remaining key of
                        Nothing ->
                            inner

                        Just rest ->
                            coverFree config ctx (wordsLeft - 1) rest (key :: picked) inner
                )
                acc


{-| Whether `wordsLeft` further words could possibly account for `remaining`.
-}
coverable : Context -> Int -> Int -> String -> Bool
coverable ctx minWordLength wordsLeft remaining =
    let
        len =
            String.length remaining
    in
    (wordsLeft > 0)
        && (len >= wordsLeft * minWordLength)
        && (len <= wordsLeft * ctx.longest)


{-| Cover the target using exactly this list of word lengths.

Lengths may be filled in any order — the rarest-letter branch decides which word
is found first — but each one keeps the position it held in the enumeration, so
an answer to `(8,7)` is reported as its eight-letter word then its seven.

-}
coverLengths : Context -> String -> List Int -> Accumulator -> Accumulator
coverLengths ctx remaining lengths acc =
    if isFull ctx acc then
        acc

    else
        coverLengthsHelp ctx (List.indexedMap Tuple.pair lengths) remaining [] acc


{-| An unfilled word of the enumeration: where it sits, and how long it is.
-}
type alias Slot =
    ( Int, Int )


coverLengthsHelp : Context -> List Slot -> String -> List ( Int, String ) -> Accumulator -> Accumulator
coverLengthsHelp ctx slots remaining picked acc =
    if isFull ctx acc then
        acc

    else if remaining == "" then
        if List.isEmpty slots then
            record (inEnumerationOrder picked) acc

        else
            acc

    else if List.isEmpty slots then
        acc

    else
        rarestBucket ctx remaining
            |> List.foldl
                (\key inner ->
                    case takeSlot (String.length key) slots of
                        Nothing ->
                            inner

                        Just ( position, remainingSlots ) ->
                            case subtract remaining key of
                                Nothing ->
                                    inner

                                Just rest ->
                                    coverLengthsHelp ctx remainingSlots rest (( position, key ) :: picked) inner
                )
                acc


inEnumerationOrder : List ( Int, String ) -> List String
inEnumerationOrder picked =
    picked
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


{-| Claim the first unfilled slot of this length, reporting where it sat.
-}
takeSlot : Int -> List Slot -> Maybe ( Int, List Slot )
takeSlot length slots =
    case slots of
        [] ->
            Nothing

        (( position, slotLength ) as slot) :: rest ->
            if slotLength == length then
                Just ( position, rest )

            else
                takeSlot length rest
                    |> Maybe.map (\( taken, remaining ) -> ( taken, slot :: remaining ))


{-| Candidates containing the least common of the remaining letters. Every
solution must contain some word covering that letter, so restricting the branch
to this bucket loses nothing and discards almost everything.
-}
rarestBucket : Context -> String -> List String
rarestBucket ctx remaining =
    case rarestLetter ctx remaining of
        Nothing ->
            []

        Just letter ->
            CoreDict.get letter ctx.buckets |> Maybe.withDefault []


rarestLetter : Context -> String -> Maybe Char
rarestLetter ctx remaining =
    remaining
        |> String.toList
        |> distinct
        |> List.foldl (keepRarer ctx) Nothing
        |> Maybe.map Tuple.first


keepRarer : Context -> Char -> Maybe ( Char, Int ) -> Maybe ( Char, Int )
keepRarer ctx candidate best =
    let
        size =
            CoreDict.get candidate ctx.sizes |> Maybe.withDefault 0
    in
    case best of
        Just ( _, bestSize ) ->
            if size < bestSize then
                Just ( candidate, size )

            else
                best

        Nothing ->
            Just ( candidate, size )


isFull : Context -> Accumulator -> Bool
isFull ctx acc =
    acc.count >= ctx.maxResults


{-| Record a combination in the order it should be shown, ignoring reorderings
of one already found.
-}
record : List String -> Accumulator -> Accumulator
record display acc =
    let
        canonical =
            display |> List.sort |> String.join " "
    in
    if Set.member canonical acc.seen then
        acc

    else
        { combos = display :: acc.combos
        , seen = Set.insert canonical acc.seen
        , count = acc.count + 1
        }



-- MULTISET ARITHMETIC


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



-- STAGE 3: EXPAND


{-| Turn a list of picked sorted-letter keys into all word-tuple combinations.
-}
expand : (String -> Bool) -> Dictionary -> List String -> List Result
expand keep dict picked =
    picked
        |> List.map (\k -> Dict.lookup k dict |> List.filter keep)
        |> cartesian


{-| Each length in an enumeration is one word, so a multi-word entry cannot fill
one: "(8,7)" is not answered by "all the go". The dictionary keeps such phrases
because they are the only route to answers containing a word too short to be
indexed, but that route only exists when no enumeration is claiming otherwise.
-}
admissible : WordLengths -> String -> Bool
admissible lengths entry =
    case lengths of
        AnyLengths ->
            True

        OneOf _ ->
            not (String.contains " " entry)


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



-- HELPERS


distinct : List comparable -> List comparable
distinct list =
    list
        |> List.foldl
            (\item ( seen, acc ) ->
                if Set.member item seen then
                    ( seen, acc )

                else
                    ( Set.insert item seen, item :: acc )
            )
            ( Set.empty, [] )
        |> Tuple.second
        |> List.reverse

