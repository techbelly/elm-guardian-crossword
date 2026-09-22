module Anagram.Dict exposing
    ( Dictionary
    , candidateKeys
    , decoder
    , fromList
    , lookup
    )

{-| The anagram dictionary: 219k words indexed by their sorted letters.

The obvious indexes — a `Dict` from key to words, and a second index of keys by
length — come to some seventy megabytes of small objects, enough to leave a
phone collecting garbage through every keystroke for the rest of the solve. So
the words are kept as the one string they arrived in, rearranged so that
everything sharing a key sits together, and the index is nothing but offsets and
bit masks in `Array Int`. That is around a tenth of the memory and no object per
word at all.

A _group_ is one key and the words that share it. Groups are held in key order,
which is what lets `lookup` bisect them. The keys themselves are never stored: a
group's key is the sorted letters of any of its words, and only the few hundred
groups that survive a search's mask test are ever asked for theirs.

-}

import Anagram.Letters exposing (sortedKey)
import Anagram.Mask as Mask exposing (Mask)
import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as D
import Set exposing (Set)


type Dictionary
    = Dictionary
        { words : String

        -- Where each group's words begin in `words`. One entry longer than the
        -- other arrays: the extra offset closes the last group.
        , groupStart : Array Int
        , keyLength : Array Int
        , maskOnce : Array Int
        , maskTwice : Array Int
        }


{-| Read the shipped word list: one word per line.
-}
decoder : D.Decoder Dictionary
decoder =
    D.map fromText D.string


{-| Build from explicit key-to-words pairs, as the tests do. Keys are taken from
the words themselves, so a pair whose key disagrees with its words is indexed
under the one its words give.
-}
fromList : List ( String, List String ) -> Dictionary
fromList entries =
    entries
        |> List.concatMap Tuple.second
        |> String.join "\n"
        |> fromText



-- LOOKING THINGS UP


{-| The keys worth considering in a search: one of the lengths the caller will
accept, and letters that fit inside the target. The scan reads the flat arrays
and builds a string only for the groups that survive, which on real input is a
few hundred out of 196k.
-}
candidateKeys : List Int -> Mask -> Dictionary -> List String
candidateKeys lengths targetMask dictionary =
    scan dictionary (Set.fromList lengths) targetMask (groupCount dictionary - 1) []


scan : Dictionary -> Set Int -> Mask -> Int -> List String -> List String
scan ((Dictionary d) as dictionary) lengths targetMask group acc =
    if group < 0 then
        acc

    else if
        Set.member (at group d.keyLength) lengths
            && Mask.subsetOf { once = at group d.maskOnce, twice = at group d.maskTwice } targetMask
    then
        scan dictionary lengths targetMask (group - 1) (keyOf group dictionary :: acc)

    else
        scan dictionary lengths targetMask (group - 1) acc


{-| The words sharing a key. Groups sit in key order, so this is a bisection,
recomputing keys on the way down rather than storing them.
-}
lookup : String -> Dictionary -> List String
lookup key dictionary =
    bisect key dictionary 0 (groupCount dictionary - 1)


bisect : String -> Dictionary -> Int -> Int -> List String
bisect key dictionary low high =
    if low > high then
        []

    else
        let
            middle =
                low + (high - low) // 2
        in
        case compare key (keyOf middle dictionary) of
            EQ ->
                wordsOf middle dictionary

            LT ->
                bisect key dictionary low (middle - 1)

            GT ->
                bisect key dictionary (middle + 1) high


groupCount : Dictionary -> Int
groupCount (Dictionary d) =
    Array.length d.keyLength


{-| A group's key, read back off its first word.
-}
keyOf : Int -> Dictionary -> String
keyOf group dictionary =
    case wordsOf group dictionary of
        first :: _ ->
            sortedKey first

        [] ->
            ""


wordsOf : Int -> Dictionary -> List String
wordsOf group (Dictionary d) =
    String.slice (at group d.groupStart) (at (group + 1) d.groupStart - 1) d.words
        |> String.split "\n"


at : Int -> Array Int -> Int
at index array =
    Array.get index array |> Maybe.withDefault 0



-- BUILDING THE INDEX


fromText : String -> Dictionary
fromText text =
    text
        |> wordsByKey
        |> assemble


{-| The words gathered under their keys, which is all the grouping needs. The
words are cut from the text once, here, and the groups are joined straight back
out of them.
-}
wordsByKey : String -> Dict String (List String)
wordsByKey text =
    let
        ( lastLineStart, gathered ) =
            String.indexes "\n" text
                |> List.foldl
                    (\break ( from, acc ) -> ( break + 1, gather (String.slice from break text) acc ))
                    ( 0, Dict.empty )
    in
    gather (String.dropLeft lastLineStart text) gathered


gather : String -> Dict String (List String) -> Dict String (List String)
gather rawWord acc =
    let
        word =
            -- A list written on Windows would otherwise carry its returns into
            -- the displayed words.
            if String.endsWith "\u{000D}" rawWord then
                String.dropRight 1 rawWord

            else
                rawWord

        key =
            sortedKey word
    in
    if String.isEmpty key then
        acc

    else
        Dict.update key (\existing -> Just (word :: Maybe.withDefault [] existing)) acc


{-| Lay the groups out in key order, so that a group's words are one slice of
the result and the index needs only where each group starts.
-}
assemble : Dict String (List String) -> Dictionary
assemble byKey =
    let
        built =
            Dict.foldl addGroup emptyBuild byKey
    in
    Dictionary
        { words = String.join "\n" (List.reverse built.texts)

        -- `next` has run past the last group, which closes it.
        , groupStart = descendingToArray (built.next :: built.starts)
        , keyLength = descendingToArray built.lengths
        , maskOnce = descendingToArray built.once
        , maskTwice = descendingToArray built.twice
        }


type alias Build =
    { texts : List String
    , starts : List Int
    , lengths : List Int
    , once : List Int
    , twice : List Int
    , next : Int
    }


emptyBuild : Build
emptyBuild =
    { texts = [], starts = [], lengths = [], once = [], twice = [], next = 0 }


addGroup : String -> List String -> Build -> Build
addGroup key words build =
    let
        groupText =
            -- Words were gathered by prepending, so this restores the order the
            -- file listed them in.
            String.join "\n" (List.reverse words)

        mask =
            Mask.fromSortedKey key
    in
    { texts = groupText :: build.texts
    , starts = build.next :: build.starts
    , lengths = String.length key :: build.lengths
    , once = mask.once :: build.once
    , twice = mask.twice :: build.twice

    -- The newline that will join this group to the next.
    , next = build.next + String.length groupText + 1
    }


descendingToArray : List Int -> Array Int
descendingToArray =
    List.reverse >> Array.fromList
