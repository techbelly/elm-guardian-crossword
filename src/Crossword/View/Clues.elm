module Crossword.View.Clues exposing (viewCluePanel, viewStickyBar)

import Crossword.Grid as Grid
import Crossword.Types
    exposing
        ( Direction(..)
        , Clue
        , ClueId
        , Grid
        , Msg(..)
        , Puzzle
        , Selection
        )
import Html exposing (Html, div, h3, li, ol, span, text)
import Html.Attributes as Attr
import Html.Events


viewStickyBar : Puzzle -> Maybe Selection -> Html Msg
viewStickyBar puzzle selection =
    case selection of
        Nothing ->
            div [ Attr.class "crossword__sticky-clue crossword__sticky-clue--empty" ] []

        Just sel ->
            case lookupClue sel.clueId puzzle of
                Nothing ->
                    div [ Attr.class "crossword__sticky-clue" ] []

                Just entry ->
                    let
                        dirLabel =
                            case entry.id.direction of
                                Across ->
                                    "across"

                                Down ->
                                    "down"
                    in
                    div [ Attr.class "crossword__sticky-clue" ]
                        [ span [ Attr.class "crossword__sticky-clue-number" ]
                            [ text (entry.humanNumber ++ " " ++ dirLabel) ]
                        , span [ Attr.class "crossword__sticky-clue-text" ]
                            [ text (" " ++ entry.text) ]
                        ]


viewCluePanel : Puzzle -> Grid -> Maybe Selection -> Html Msg
viewCluePanel puzzle grid selection =
    let
        activeGroup =
            selection
                |> Maybe.andThen (\sel -> lookupClue sel.clueId puzzle)
                |> Maybe.map .group
                |> Maybe.withDefault []

        acrossEntries =
            puzzle.clues
                |> List.filter (\e -> e.id.direction == Across)
                |> List.sortBy (\e -> e.id.number)

        downEntries =
            puzzle.clues
                |> List.filter (\e -> e.id.direction == Down)
                |> List.sortBy (\e -> e.id.number)
    in
    div [ Attr.class "crossword__clues" ]
        [ viewClueSection "Across" acrossEntries grid activeGroup
        , viewClueSection "Down" downEntries grid activeGroup
        ]


viewClueSection : String -> List Clue -> Grid -> List ClueId -> Html Msg
viewClueSection title entries grid activeGroup =
    div [ Attr.class ("crossword__clues-section crossword__clues-section--" ++ String.toLower title) ]
        [ h3 [ Attr.class "crossword__clues-header" ] [ text title ]
        , ol [ Attr.class "crossword__clues-list" ]
            (List.map (\entry -> viewClueItem entry grid activeGroup) entries)
        ]


viewClueItem : Clue -> Grid -> List ClueId -> Html Msg
viewClueItem entry grid activeGroup =
    let
        isSelected =
            List.any (\eid -> eid == entry.id) activeGroup

        isAnswered =
            Grid.isClueAnswered entry grid

        classes =
            [ ( "crossword__clue", True )
            , ( "crossword__clue--selected", isSelected )
            , ( "crossword__clue--answered", isAnswered )
            ]
                |> List.filter Tuple.second
                |> List.map Tuple.first
                |> String.join " "
    in
    li
        [ Attr.class classes
        , Attr.id (clueElementId entry.id)
        , Html.Events.onClick (ClueClicked entry.id)
        ]
        [ span [ Attr.class "crossword__clue-number" ] [ text entry.humanNumber ]
        , span [ Attr.class "crossword__clue-text" ] [ text (" " ++ entry.text) ]
        ]


clueElementId : ClueId -> String
clueElementId cid =
    let
        dir =
            case cid.direction of
                Across ->
                    "across"

                Down ->
                    "down"
    in
    "clue-" ++ String.fromInt cid.number ++ "-" ++ dir


lookupClue : ClueId -> Puzzle -> Maybe Clue
lookupClue cid puzzle =
    puzzle.clues
        |> List.filter (\c -> c.id == cid)
        |> List.head
