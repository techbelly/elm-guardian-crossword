module Crossword.View.App exposing (view)

import Crossword.Keyboard as Keyboard
import Crossword.Types
    exposing
        ( ActiveModel
        , Model(..)
        , Msg(..)
        , NavigationStyle(..)
        )
import Crossword.View.Clues as ViewClues
import Crossword.View.Grid as ViewGrid
import Crossword.View.Title as ViewTitle
import Html exposing (Html, div, h1, input, label, text)
import Html.Attributes as Attr
import Html.Events
import Json.Decode


view : Model -> Html Msg
view model =
    case model of
        Failed err ->
            errorScreen err

        Active activeModel ->
            crosswordDisplay activeModel


errorScreen : String -> Html Msg
errorScreen err =
    div []
        [ h1 [] [ text "Error loading crossword" ]
        , Html.pre [] [ text err ]
        ]


crosswordDisplay : ActiveModel -> Html Msg
crosswordDisplay model =
    div
        [ Attr.class "crossword"
        , Attr.tabindex 0
        , Html.Events.preventDefaultOn "keydown"
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
        [ ViewTitle.viewTitle model.puzzle
        , ViewClues.viewStickyBar model.puzzle model.selection
        , div [ Attr.class "crossword__content" ]
            [ ViewGrid.viewGrid model.puzzle model.grid model.selection
            , ViewClues.viewCluePanel model.puzzle model.grid model.selection
            ]
        , viewNavigationToggle model.navigationStyle
        ]


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
