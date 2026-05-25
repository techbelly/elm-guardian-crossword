module Anagram.Modal exposing (view)

import Crossword.Types
    exposing
        ( AnagramModalData
        , AnagramModalState(..)
        , AnagramSearchOutcome(..)
        , DictionaryState(..)
        , Msg(..)
        )
import Html exposing (Html, button, div, form, h2, input, li, p, text, ul)
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
                [ input
                    [ Attr.class "anagram-modal__input"
                    , Attr.type_ "text"
                    , Attr.value data.input
                    , Attr.autofocus True
                    , Attr.placeholder "Letters to anagram"
                    , Html.Events.onInput AnagramInputChanged
                    ]
                    []
                , button
                    [ Attr.class "anagram-modal__submit"
                    , Attr.type_ "submit"
                    , Attr.disabled (submitDisabled dictState data)
                    ]
                    [ text (submitLabel dictState data) ]
                ]
            , viewBody dictState data
            ]
        ]


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
            "Find anagrams"


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
            statusMessage "Type letters and press enter."

        Just AnagramSearching ->
            statusMessage "Looking for anagrams…"

        Just AnagramTooShort ->
            statusMessage "Need at least 3 letters."

        Just AnagramTooLong ->
            statusMessage "Too many letters — limit is 15."

        Just AnagramNoResults ->
            statusMessage "No anagrams found."

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
