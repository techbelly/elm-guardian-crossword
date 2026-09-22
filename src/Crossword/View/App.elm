module Crossword.View.App exposing (letterInputId, view)

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
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode


{-| The solving container. main.js looks for it by this id when deciding
whether a tap landed on the puzzle.
-}
gridElementId : String
gridElementId =
    "crossword"


{-| Letters are typed into a real text input rather than into the container:
a focused input is the only thing that raises the on-screen keyboard on a
phone. It is offscreen, so main.js focuses it when a cell or clue is tapped,
and focus has to be put back after the anagram modal takes it away.
-}
letterInputId : String
letterInputId =
    "crossword-letter-input"


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
    in
    div
        [ Attr.class "crossword"
        , Attr.id gridElementId
        ]
        [ viewLetterInput modalOpen
        , div [ Attr.class "crossword__top" ]
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


{-| Both halves of typing land here: hardware keys through keydown, and soft
keyboards that report "Unidentified" through the input event, which carries
the characters themselves. Keys we act on are prevented, so a key never
arrives twice.
-}
viewLetterInput : Bool -> Html Msg
viewLetterInput modalOpen =
    input
        ([ Attr.id letterInputId
         , Attr.class "crossword__letter-input"
         , Attr.type_ "text"
         , Attr.value ""
         , Attr.attribute "autocomplete" "off"
         , Attr.attribute "autocorrect" "off"
         , Attr.attribute "autocapitalize" "characters"
         , Attr.attribute "spellcheck" "false"
         , Attr.attribute "enterkeyhint" "next"
         , Attr.attribute "aria-label" "Type letters into the selected square"
         ]
            ++ (if modalOpen then
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
                    , Html.Events.on "input"
                        (Json.Decode.map TextEntered
                            (Json.Decode.at [ "target", "value" ] Json.Decode.string)
                        )
                    ]
               )
        )
        []


viewBackLink : Html Msg
viewBackLink =
    button
        [ Attr.class "crossword__back"
        , Attr.type_ "button"
        , Html.Events.onClick BackToLanding
        ]
        [ text "←"
        , span [ Attr.class "crossword__back-label" ] [ text " All crosswords" ]
        ]


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
