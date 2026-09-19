module Anagram.Fodder exposing
    ( Token
    , letters
    , letterCount
    , toggle
    , tokenise
    )

{-| The words of a clue, each switched on or off as anagram fodder.

Cryptic fodder is usually a word or two out of the clue, so tokens start
switched off and are picked rather than pruned. Fodder that isn't a whole clue
word — an abbreviation, a letter borrowed from "heart of Rome" — has no token to
click, which is what the modal's free-text field is for.

-}

import Anagram.Letters as Letters


type alias Token =
    { text : String
    , included : Bool
    }


{-| Split clue text into switchable tokens, dropping anything with no letters in
it. The trailing enumeration `(8)` falls out for free, as do bare punctuation
tokens; surrounding punctuation is trimmed off for display while apostrophes and
hyphens inside a word survive.
-}
tokenise : String -> List Token
tokenise text =
    text
        |> String.words
        |> List.map trimPunctuation
        |> List.filter (\word -> String.any Char.isAlpha word)
        |> List.map (\word -> { text = word, included = False })


trimPunctuation : String -> String
trimPunctuation word =
    word
        |> String.toList
        |> dropWhile isPunctuation
        |> List.reverse
        |> dropWhile isPunctuation
        |> List.reverse
        |> String.fromList


isPunctuation : Char -> Bool
isPunctuation c =
    not (Char.isAlphaNum c)


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: rest ->
            if predicate x then
                dropWhile predicate rest

            else
                list


toggle : Int -> List Token -> List Token
toggle index tokens =
    List.indexedMap
        (\i token ->
            if i == index then
                { token | included = not token.included }

            else
                token
        )
        tokens


{-| The letters the search will run on: every switched-on token plus whatever
was typed into the extras field.
-}
letters : String -> List Token -> String
letters extra tokens =
    tokens
        |> List.filter .included
        |> List.map .text
        |> String.concat
        |> (\picked -> picked ++ extra)
        |> Letters.sanitise


letterCount : String -> List Token -> Int
letterCount extra tokens =
    String.length (letters extra tokens)
