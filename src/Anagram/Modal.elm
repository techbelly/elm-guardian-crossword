module Anagram.Modal exposing (view)

import Anagram.Enumeration as Enumeration
import Anagram.Fodder as Fodder exposing (Token)
import Crossword.Types
    exposing
        ( AnagramModalData
        , AnagramModalState(..)
        , AnagramSearchOutcome(..)
        , DictionaryState(..)
        , Msg(..)
        )
import Html exposing (Html, button, div, form, h2, input, li, p, span, text, ul)
import Html.Attributes as Attr
import Html.Events
import Json.Decode


view : DictionaryState -> AnagramModalState -> Html Msg
view dictState modalState =
    case modalState of
        AnagramClosed ->
            text ""

        AnagramOpen data ->
            viewModal dictState data


viewModal : DictionaryState -> AnagramModalData -> Html Msg
viewModal dictState data =
    div
        [ Attr.class "anagram-modal__backdrop"
        , Html.Events.onClick CloseAnagramModal
        ]
        [ div
            [ Attr.class "anagram-modal"

            -- Stop clicks inside the modal panel bubbling to the backdrop
            , Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( NoopClick, True ))
            ]
            [ div [ Attr.class "anagram-modal__header" ]
                [ h2 [ Attr.class "anagram-modal__title" ] [ text "Anagrams" ]
                , button
                    [ Attr.class "anagram-modal__close"
                    , Attr.type_ "button"
                    , Attr.attribute "aria-label" "Close"
                    , Html.Events.onClick CloseAnagramModal
                    ]
                    [ text "×" ]
                ]
            , form
                [ Attr.class "anagram-modal__form"
                , Html.Events.onSubmit AnagramSubmit
                ]
                [ viewTokens data.tokens
                , div [ Attr.class "anagram-modal__fields" ]
                    [ viewField "extra" "Extra letters"
                        [ Attr.class "anagram-modal__input"
                        , Attr.type_ "text"
                        , Attr.value data.extra
                        , Attr.placeholder "abbreviations, odd letters…"
                        , Html.Events.onInput AnagramExtraChanged
                        ]
                    , viewField "lengths" "Lengths"
                        [ Attr.class "anagram-modal__input"
                        , Attr.type_ "text"
                        , Attr.value data.enumeration
                        , Attr.placeholder "4,7"
                        , Html.Events.onInput AnagramEnumerationChanged
                        ]
                    ]
                , div [ Attr.class "anagram-modal__actions" ]
                    [ viewTally data
                    , button
                        [ Attr.class "anagram-modal__submit"
                        , Attr.disabled (submitDisabled dictState data)
                        ]
                        [ text (submitLabel dictState data) ]
                    ]
                ]
            , viewBody dictState data
            ]
        ]


viewField : String -> String -> List (Html.Attribute Msg) -> Html Msg
viewField modifier label attrs =
    Html.label [ Attr.class ("anagram-modal__field anagram-modal__field--" ++ modifier) ]
        [ span [ Attr.class "anagram-modal__field-label" ] [ text label ]
        , input attrs []
        ]


viewTokens : List Token -> Html Msg
viewTokens tokens =
    if List.isEmpty tokens then
        p [ Attr.class "anagram-modal__status" ]
            [ text "No clue selected — type the letters below." ]

    else
        div [ Attr.class "anagram-modal__tokens" ]
            (List.indexedMap viewToken tokens)


viewToken : Int -> Token -> Html Msg
viewToken index token =
    button
        [ Attr.class "anagram-modal__token"
        , Attr.classList [ ( "anagram-modal__token--on", token.included ) ]
        , Attr.type_ "button"
        , Attr.attribute "aria-pressed"
            (if token.included then
                "true"

             else
                "false"
            )
        , Html.Events.onClick (AnagramTokenToggled index)
        ]
        [ text token.text ]



-- TALLY


{-| Letters picked so far, measured against the enumeration when there is one.
Mismatches are flagged here rather than left to surface as an empty result list.
-}
viewTally : AnagramModalData -> Html Msg
viewTally data =
    let
        count =
            Fodder.letterCount data.extra data.tokens

        letterLabel =
            String.fromInt count
                ++ (if count == 1 then
                        " letter"

                    else
                        " letters"
                   )
    in
    case enumerationNote data count of
        Nothing ->
            span [ Attr.class "anagram-modal__tally" ] [ text letterLabel ]

        Just ( note, isProblem ) ->
            span
                [ Attr.class "anagram-modal__tally"
                , Attr.classList [ ( "anagram-modal__tally--problem", isProblem ) ]
                ]
                [ text (letterLabel ++ " · " ++ note) ]


enumerationNote : AnagramModalData -> Int -> Maybe ( String, Bool )
enumerationNote data count =
    if String.isEmpty (String.trim data.enumeration) then
        Nothing

    else
        case Enumeration.parse data.enumeration of
            Nothing ->
                Just ( "lengths not understood", True )

            Just enumeration ->
                let
                    wanted =
                        Enumeration.totalLetters enumeration
                in
                if wanted == count then
                    Nothing

                else
                    Just ( "lengths need " ++ String.fromInt wanted, True )



-- SUBMIT


submitDisabled : DictionaryState -> AnagramModalData -> Bool
submitDisabled dictState data =
    case dictState of
        DictReady _ ->
            data.lastSearch == Just AnagramSearching

        _ ->
            True


submitLabel : DictionaryState -> AnagramModalData -> String
submitLabel dictState data =
    case ( dictState, data.lastSearch ) of
        ( DictLoading, _ ) ->
            "Loading dictionary…"

        ( DictNotLoaded, _ ) ->
            "Loading dictionary…"

        ( DictFailed _, _ ) ->
            "Dictionary failed"

        ( DictReady _, Just AnagramSearching ) ->
            "Searching…"

        _ ->
            "Solve"


viewBody : DictionaryState -> AnagramModalData -> Html Msg
viewBody dictState data =
    case dictState of
        DictLoading ->
            statusMessage "Loading dictionary…"

        DictNotLoaded ->
            -- Shouldn't appear in practice — opening triggers the load — but render defensively
            statusMessage "Loading dictionary…"

        DictFailed err ->
            div [ Attr.class "anagram-modal__status anagram-modal__status--error" ]
                [ p [] [ text ("Failed to load dictionary: " ++ err) ]
                , button
                    [ Attr.class "anagram-modal__retry"
                    , Attr.type_ "button"
                    , Html.Events.onClick OpenAnagramModal
                    ]
                    [ text "Retry" ]
                ]

        DictReady _ ->
            viewResults data


viewResults : AnagramModalData -> Html Msg
viewResults data =
    case data.lastSearch of
        Nothing ->
            statusMessage "Pick the fodder words and press solve."

        Just AnagramSearching ->
            statusMessage "Looking for anagrams…"

        Just AnagramTooShort ->
            statusMessage "Need at least 3 letters."

        Just AnagramTooLong ->
            statusMessage "Too many letters — limit is 15."

        Just AnagramNoResults ->
            statusMessage "No anagrams found."

        Just AnagramNoResultsForLengths ->
            statusMessage "No anagrams with those lengths — clear the lengths field to search freely."

        Just (AnagramResults results) ->
            ul [ Attr.class "anagram-modal__results" ]
                (List.map viewResult results)


viewResult : List String -> Html Msg
viewResult words =
    li [ Attr.class "anagram-modal__result" ]
        [ text (String.join " · " words) ]


statusMessage : String -> Html Msg
statusMessage msg =
    div [ Attr.class "anagram-modal__status" ]
        [ p [] [ text msg ] ]
