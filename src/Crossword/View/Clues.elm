module Crossword.View.Clues exposing (clueElementId, viewCluePanel, viewStickyBar)

import Crossword.Grid as Grid
import Crossword.Types
    exposing
        ( ActiveModel
        , Direction(..)
        , Clue
        , ClueId
        , Grid
        , Msg(..)
        , lookupClue
        )
import Html exposing (Html, div, h3, li, ol, span, text)
import Html.Attributes as Attr
import Html.Events


directionLabel : Direction -> String
directionLabel dir =
    case dir of
        Across ->
            "across"

        Down ->
            "down"


viewStickyBar : ActiveModel -> Html Msg
viewStickyBar model =
    case model.selection of
        Nothing ->
            div [ Attr.class "crossword__sticky-clue crossword__sticky-clue--empty" ] []

        Just sel ->
            case lookupClue sel.clueId model.puzzle of
                Nothing ->
                    div [ Attr.class "crossword__sticky-clue" ] []

                Just clue ->
                    div [ Attr.class "crossword__sticky-clue" ]
                        [ span [ Attr.class "crossword__sticky-clue-number" ]
                            [ text (clue.humanNumber ++ " " ++ directionLabel clue.id.direction) ]
                        , span [ Attr.class "crossword__sticky-clue-text" ]
                            [ text (" " ++ clue.text) ]
                        ]


viewCluePanel : ActiveModel -> Html Msg
viewCluePanel model =
    let
        activeGroup =
            model.selection
                |> Maybe.andThen (\sel -> lookupClue sel.clueId model.puzzle)
                |> Maybe.map .group
                |> Maybe.withDefault []

        acrossClues =
            model.puzzle.clues
                |> List.filter (\c -> c.id.direction == Across)

        downClues =
            model.puzzle.clues
                |> List.filter (\c -> c.id.direction == Down)
    in
    div [ Attr.class "crossword__clues" ]
        [ viewClueSection "Across" acrossClues model.grid activeGroup
        , viewClueSection "Down" downClues model.grid activeGroup
        ]


viewClueSection : String -> List Clue -> Grid -> List ClueId -> Html Msg
viewClueSection title clues grid activeGroup =
    div [ Attr.class ("crossword__clues-section crossword__clues-section--" ++ String.toLower title) ]
        [ h3 [ Attr.class "crossword__clues-header" ] [ text title ]
        , ol [ Attr.class "crossword__clues-list" ]
            (List.map (\clue -> viewClueItem clue grid activeGroup) clues)
        ]


viewClueItem : Clue -> Grid -> List ClueId -> Html Msg
viewClueItem clue grid activeGroup =
    let
        isSelected =
            List.member clue.id activeGroup

        isAnswered =
            Grid.isClueAnswered clue grid

    in
    li
        [ Attr.classList
            [ ( "crossword__clue", True )
            , ( "crossword__clue--selected", isSelected )
            , ( "crossword__clue--answered", isAnswered )
            ]
        , Attr.id (clueElementId clue.id)
        , Html.Events.onClick (ClueClicked clue.id)
        ]
        [ span [ Attr.class "crossword__clue-number" ] [ text clue.humanNumber ]
        , span [ Attr.class "crossword__clue-text" ] [ text (" " ++ clue.text) ]
        ]


clueElementId : ClueId -> String
clueElementId cid =
    "clue-" ++ String.fromInt cid.number ++ "-" ++ directionLabel cid.direction
