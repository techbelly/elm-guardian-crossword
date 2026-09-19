module AnagramFodderTest exposing (suite)

import Anagram.Enumeration as Enumeration
import Anagram.Fodder as Fodder
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Anagram fodder and enumeration"
        [ describe "tokenise"
            [ test "splits words and starts them all switched off" <|
                \_ ->
                    Fodder.tokenise "Confused starlet (8)"
                        |> Expect.equal
                            [ { text = "Confused", included = False }
                            , { text = "starlet", included = False }
                            ]
            , test "trims surrounding punctuation but keeps it inside a word" <|
                \_ ->
                    Fodder.tokenise "\"setter's\", odd—"
                        |> List.map .text
                        |> Expect.equal [ "setter's", "odd" ]
            , test "drops tokens with no letters" <|
                \_ ->
                    Fodder.tokenise "one 2 — three (4,5)"
                        |> List.map .text
                        |> Expect.equal [ "one", "three" ]
            ]
        , describe "letters"
            [ test "only switched-on tokens contribute" <|
                \_ ->
                    Fodder.tokenise "Confused starlet jewel"
                        |> Fodder.toggle 1
                        |> Fodder.letters ""
                        |> Expect.equal "starlet"
            , test "extras are appended and sanitised" <|
                \_ ->
                    Fodder.tokenise "Confused starlet jewel"
                        |> Fodder.toggle 1
                        |> Fodder.letters "G.I."
                        |> Expect.equal "starletgi"
            , test "toggling twice switches back off" <|
                \_ ->
                    Fodder.tokenise "one two"
                        |> Fodder.toggle 0
                        |> Fodder.toggle 0
                        |> Fodder.letters ""
                        |> Expect.equal ""
            ]
        , describe "enumeration parsing"
            [ test "reads the bracketed count off the end of a clue" <|
                \_ ->
                    Enumeration.fromClue "Confused starlet picked her own jewel (8)"
                        |> Maybe.map Enumeration.toText
                        |> Expect.equal (Just "8")
            , test "takes the last bracketed group when the clue has others" <|
                \_ ->
                    Enumeration.fromClue "Nice (French) spot (4,5)"
                        |> Maybe.map Enumeration.toText
                        |> Expect.equal (Just "4,5")
            , test "no enumeration when the clue doesn't end in brackets" <|
                \_ ->
                    Enumeration.fromClue "Confused starlet" |> Expect.equal Nothing
            , test "rejects non-numeric contents" <|
                \_ ->
                    Enumeration.fromClue "Two words (2 words)" |> Expect.equal Nothing
            , test "accepts bare or bracketed text in the editable field" <|
                \_ ->
                    ( Enumeration.parse "4,7", Enumeration.parse "(4, 7)" )
                        |> Expect.equal ( Enumeration.parse "4,7", Enumeration.parse "4,7" )
            ]
        , describe "enumeration alternatives"
            [ test "a plain enumeration has exactly one reading" <|
                \_ ->
                    Enumeration.parse "4,7"
                        |> Maybe.map Enumeration.alternatives
                        |> Expect.equal (Just [ [ 4, 7 ] ])
            , test "a hyphenated run may be split or joined" <|
                \_ ->
                    Enumeration.parse "3-4"
                        |> Maybe.map Enumeration.alternatives
                        |> Expect.equal (Just [ [ 3, 4 ], [ 7 ] ])
            , test "readings multiply across hyphenated runs" <|
                \_ ->
                    Enumeration.parse "1-2,8"
                        |> Maybe.map Enumeration.alternatives
                        |> Expect.equal (Just [ [ 1, 2, 8 ], [ 3, 8 ] ])
            , test "total letters is the same whichever reading is taken" <|
                \_ ->
                    Enumeration.parse "1-2,8"
                        |> Maybe.map Enumeration.totalLetters
                        |> Expect.equal (Just 11)
            ]
        ]
