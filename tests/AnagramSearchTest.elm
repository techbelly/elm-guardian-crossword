module AnagramSearchTest exposing (suite)

import Anagram.Dict as Dict exposing (Dictionary)
import Anagram.Search as Search
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Anagram.Search"
        [ describe "sanitise"
            [ test "lowercases and strips non-letters" <|
                \_ ->
                    Search.sanitise "Confused starlet, picked her own jewel (8)"
                        |> Expect.equal "confusedstarletpickedherownjewel"
            , test "empty after stripping" <|
                \_ ->
                    Search.sanitise "()!?"
                        |> Expect.equal ""
            ]
        , describe "sortedKey"
            [ test "letters sorted" <|
                \_ -> Search.sortedKey "STARLET" |> Expect.equal "aelrstt"
            , test "ignores spaces and case in phrases" <|
                \_ -> Search.sortedKey "New York" |> Expect.equal "eknorwy"
            ]
        , describe "search — single word"
            [ test "finds all permutations sharing the sorted key" <|
                \_ ->
                    Search.search Search.defaults seatDict "seat"
                        |> Expect.equal [ [ "EATS" ], [ "SEAT" ], [ "TEAS" ] ]
            , test "alphabetical within longest-word tier" <|
                \_ ->
                    Search.search Search.defaults seatDict "ates"
                        |> Expect.equal [ [ "EATS" ], [ "SEAT" ], [ "TEAS" ] ]
            ]
        , describe "search — must use all letters"
            [ test "rejects sub-anagrams" <|
                \_ ->
                    -- "eat" is in the dict but is shorter than "seat" — should not appear
                    Search.search Search.defaults dictWithEat "seat"
                        |> List.member [ "EAT" ]
                        |> Expect.equal False
            ]
        , describe "search — multi-word"
            [ test "finds two-word combinations" <|
                \_ ->
                    -- input "lemonpea" anagrams to "POLE" + "MEAN" (8 letters)
                    Search.search Search.defaults twoWordDict "lemonpea"
                        |> Expect.equal [ [ "MEAN", "POLE" ] ]
            , test "no duplicate from swapped ordering" <|
                \_ ->
                    -- both [POLE,MEAN] and [MEAN,POLE] would be wrong — only one canonical form
                    Search.search Search.defaults twoWordDict "lemonpea"
                        |> List.length
                        |> Expect.equal 1
            ]
        , describe "ranking"
            [ test "single-word results come before multi-word" <|
                \_ ->
                    Search.search Search.defaults mixedDict "starlet"
                        -- has 7-letter singles AND 4+3 multi-words; singles must come first
                        |> List.head
                        |> Maybe.map List.length
                        |> Expect.equal (Just 1)
            ]
        , describe "config caps"
            [ test "maxResults caps the returned list" <|
                \_ ->
                    Search.search { defaults | maxResults = 2 } seatDict "seat"
                        |> List.length
                        |> Expect.equal 2
            , test "minWordLength excludes short words" <|
                \_ ->
                    -- "AT" + "ES" would be a 2+2 combination of "ates" — must not appear with minWordLength=3
                    Search.search defaults shortWordDict "ates"
                        |> List.member [ "AT", "ES" ]
                        |> Expect.equal False
            , test "maxWords caps combination size" <|
                \_ ->
                    -- "abcabc" can be made from "ABC"+"ABC" (2 words) but with maxWords=1 only single-word
                    Search.search { defaults | maxWords = 1 } abcDict "abcabc"
                        |> Expect.equal []
            ]
        , describe "search — word lengths"
            [ test "an enumeration keeps only combinations with those lengths" <|
                \_ ->
                    -- "starlet" can be one 7-letter word or STAR + LET; (4,3) admits only the pair
                    Search.search { defaults | lengths = Search.OneOf [ [ 4, 3 ] ] } mixedDict "starlet"
                        |> Expect.equal [ [ "RATS", "LET" ], [ "STAR", "LET" ], [ "TARS", "LET" ] ]
            , test "lengths match in any order" <|
                \_ ->
                    Search.search { defaults | lengths = Search.OneOf [ [ 3, 4 ] ] } mixedDict "starlet"
                        |> List.length
                        |> Expect.equal 3
            , test "words are reported in the order the enumeration gives" <|
                \_ ->
                    -- (3,4) is the same pairing as (4,3), read out the other way round
                    Search.search { defaults | lengths = Search.OneOf [ [ 3, 4 ] ] } mixedDict "starlet"
                        |> Expect.equal [ [ "LET", "RATS" ], [ "LET", "STAR" ], [ "LET", "TARS" ] ]
            , test "alternatives are tried in turn" <|
                \_ ->
                    -- how a hyphenated (4-3) reaches the search: either two words or one of 7
                    Search.search { defaults | lengths = Search.OneOf [ [ 4, 3 ], [ 7 ] ] } mixedDict "starlet"
                        |> List.length
                        |> Expect.equal 6
            , test "a phrase cannot fill one of the enumeration's words" <|
                \_ ->
                    -- "on sight" has the letters for a 7, but an enumerated 7 is one word
                    Search.search { defaults | lengths = Search.OneOf [ [ 7 ] ] } phraseDict "onsight"
                        |> Expect.equal [ [ "HOGTIES" ] ]
            , test "a phrase is fair game when no enumeration says otherwise" <|
                \_ ->
                    Search.search defaults phraseDict "onsight"
                        |> List.member [ "on sight" ]
                        |> Expect.equal True
            , test "an enumeration that doesn't total the input finds nothing" <|
                \_ ->
                    Search.search { defaults | lengths = Search.OneOf [ [ 4, 4 ] ] } mixedDict "starlet"
                        |> Expect.equal []
            , test "word lengths ignore maxWords" <|
                \_ ->
                    Search.search { defaults | maxWords = 1, lengths = Search.OneOf [ [ 4, 3 ] ] } mixedDict "starlet"
                        |> List.length
                        |> Expect.equal 3
            ]
        , describe "edge cases"
            [ test "empty input returns empty" <|
                \_ -> Search.search defaults seatDict "" |> Expect.equal []
            , test "input with no anagrams returns empty" <|
                \_ -> Search.search defaults seatDict "xyz" |> Expect.equal []
            , test "input with non-letters is sanitised first" <|
                \_ ->
                    Search.search defaults seatDict "SEAT!"
                        |> List.length
                        |> Expect.equal 3
            ]
        ]



-- TEST FIXTURES


defaults : Search.Config
defaults =
    Search.defaults


seatDict : Dictionary
seatDict =
    Dict.fromList
        [ ( "aest", [ "EATS", "SEAT", "TEAS" ] )
        ]


dictWithEat : Dictionary
dictWithEat =
    Dict.fromList
        [ ( "aet", [ "EAT" ] )
        , ( "aest", [ "SEAT" ] )
        ]


twoWordDict : Dictionary
twoWordDict =
    -- POLE = elop, MEAN = aemn — together = aeelmnop
    -- input "lemonpea" sorts to aeelmnop ✓
    Dict.fromList
        [ ( "elop", [ "POLE" ] )
        , ( "aemn", [ "MEAN" ] )
        ]


mixedDict : Dictionary
mixedDict =
    -- "starlet" sorts to aelrstt. Single-word: STARLET, RATTLES. Three+four: LET (elt) + RATS (arst) doesn't work
    -- Let's use simpler: STARTLE/STARLET/RATTLES single; LATER (aelrt) + ST? Need 7-letter sum so 4+3.
    -- 7 letters = aelrstt. Try: STAR (arst) + LET (elt) — aelrstt ✓
    Dict.fromList
        [ ( "aelrstt", [ "RATTLES", "STARLET", "STARTLE" ] )
        , ( "arst", [ "RATS", "STAR", "TARS" ] )
        , ( "elt", [ "LET" ] )
        ]


shortWordDict : Dictionary
shortWordDict =
    Dict.fromList
        [ ( "at", [ "AT" ] )
        , ( "es", [ "ES" ] )
        , ( "aest", [ "SEAT", "EATS", "TEAS" ] )
        ]


phraseDict : Dictionary
phraseDict =
    -- "on sight" and "hogties" share the key ghinost
    Dict.fromList
        [ ( "ghinost", [ "HOGTIES", "on sight" ] )
        ]


abcDict : Dictionary
abcDict =
    Dict.fromList
        [ ( "abc", [ "ABC" ] )
        ]
