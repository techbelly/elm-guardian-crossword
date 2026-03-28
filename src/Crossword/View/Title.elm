module Crossword.View.Title exposing (viewTitle)

import Crossword.Types exposing (Puzzle)
import Html exposing (Html, h1, span, text)
import Html.Attributes as Attr


viewTitle : Puzzle -> Html msg
viewTitle puzzle =
    h1 [ Attr.class "crossword__title" ]
        (text puzzle.name
            :: (case puzzle.setter of
                    Just setter ->
                        [ span
                            [ Attr.style "font-weight" "normal"
                            , Attr.style "font-style" "italic"
                            ]
                            [ text (" by " ++ setter) ]
                        ]

                    Nothing ->
                        []
               )
        )
