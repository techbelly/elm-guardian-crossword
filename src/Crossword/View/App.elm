module Crossword.View.App exposing (view)

import Anagram.Modal as AnagramModal
import Crossword.Keyboard as Keyboard
import Crossword.Types
    exposing
        ( ActiveModel
        , AnagramModalState(..)
        , Model(..)
        , Msg(..)
        , NavigationStyle(..)
        )
import Crossword.View.Clues as ViewClues
import Crossword.View.Grid as ViewGrid
import Crossword.View.Landing as ViewLanding
import Crossword.View.Title as ViewTitle
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode


view : Model -> Html Msg
view model =
    case model of
        Landing landingModel ->
            ViewLanding.view landingModel

        Active activeModel ->
            crosswordDisplay activeModel


crosswordDisplay : ActiveModel -> Html Msg
crosswordDisplay model =
    let
        modalOpen =
            case model.anagramModal of
                AnagramOpen _ ->
                    True

                AnagramClosed ->
                    False

        keyHandlerAttrs =
            if modalOpen then
                []

            else
                [ Html.Events.preventDefaultOn "keydown"
                    (Json.Decode.map2
                        (\key shift ->
                            ( KeyPressed key shift
                            , Keyboard.shouldPreventDefault key
                            )
                        )
                        (Json.Decode.field "key" Json.Decode.string)
                        (Json.Decode.field "shiftKey" Json.Decode.bool)
                    )
                ]
    in
    div
        ([ Attr.class "crossword"
         , Attr.tabindex 0
         ]
            ++ keyHandlerAttrs
        )
        [ div [ Attr.class "crossword__top" ]
            [ viewBackLink
            , ViewTitle.viewTitle model.puzzle
            , viewAnagramButton
            ]
        , ViewClues.viewStickyBar model
        , div [ Attr.class "crossword__content" ]
            [ ViewGrid.viewGrid model
            , ViewClues.viewCluePanel model
            ]
        , viewNavigationToggle model.navigationStyle
        , AnagramModal.view model.dictionary model.anagramModal
        ]


viewBackLink : Html Msg
viewBackLink =
    button
        [ Attr.class "crossword__back"
        , Attr.type_ "button"
        , Html.Events.onClick BackToLanding
        ]
        [ text "← All crosswords" ]


viewAnagramButton : Html Msg
viewAnagramButton =
    button
        [ Attr.class "crossword__anagram-button"
        , Attr.type_ "button"
        , Attr.attribute "aria-label" "Open anagram finder"
        , Attr.title "Anagram finder"
        , Html.Events.onClick OpenAnagramModal
        ]
        [ text "ARTS↔TSAR" ]


viewNavigationToggle : NavigationStyle -> Html Msg
viewNavigationToggle style =
    div [ Attr.class "crossword__nav-toggle" ]
        [ text "Navigation: "
        , navRadio style Guardian "Guardian"
        , navRadio style NYT "NYT"
        ]


navRadio : NavigationStyle -> NavigationStyle -> String -> Html Msg
navRadio current option labelText =
    label [ Attr.class "nav-toggle__label" ]
        [ input
            [ Attr.type_ "radio"
            , Attr.name "nav-style"
            , Attr.class "nav-toggle__radio"
            , Attr.checked (current == option)
            , Html.Events.onClick (SetNavigation option)
            ]
            []
        , text labelText
        ]
