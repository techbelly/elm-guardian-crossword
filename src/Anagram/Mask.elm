module Anagram.Mask exposing
    ( Mask
    , fromSortedKey
    , subsetOf
    )

{-| A cheap necessary condition for multiset containment.

Testing "are these letters available in the target?" exactly means walking two
sorted strings. Doing that for all 217k dictionary keys on every search is the
bottleneck, so each key carries a pair of 26-bit masks instead: one bit per
letter appearing at least once, one bit per letter appearing at least twice.

Containment implies both masks are subsets, so three integer operations reject
the overwhelming majority of keys. On real inputs this leaves 500–1200 keys out
of 217k, which the exact check can then afford to walk.

-}

import Bitwise


type alias Mask =
    { once : Int
    , twice : Int
    }


{-| Build the masks from a sorted-letter key. Relies on the key being sorted so
a repeat is always the previous character; non a–z characters are ignored.
-}
fromSortedKey : String -> Mask
fromSortedKey key =
    let
        final =
            String.foldl accumulate { prev = ' ', once = 0, twice = 0 } key
    in
    { once = final.once, twice = final.twice }


type alias Accumulator =
    { prev : Char
    , once : Int
    , twice : Int
    }


accumulate : Char -> Accumulator -> Accumulator
accumulate c acc =
    let
        code =
            Char.toCode c - 97
    in
    if code < 0 || code > 25 then
        acc

    else
        let
            bit =
                Bitwise.shiftLeftBy code 1
        in
        { prev = c
        , once = Bitwise.or acc.once bit
        , twice =
            if c == acc.prev then
                Bitwise.or acc.twice bit

            else
                acc.twice
        }


{-| True when every bit of `inner` is set in `outer`. Necessary but not
sufficient for the letters of `inner` to fit inside `outer`.
-}
subsetOf : Mask -> Mask -> Bool
subsetOf inner outer =
    (Bitwise.and inner.once (Bitwise.complement outer.once) == 0)
        && (Bitwise.and inner.twice (Bitwise.complement outer.twice) == 0)
