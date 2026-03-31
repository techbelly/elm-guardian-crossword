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
import Html exposing (Html, b, div, h3, i, li, ol, span, text)
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
                            (text " " :: viewClueHtml clue.text)
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
        , span [ Attr.class "crossword__clue-text" ] (text " " :: viewClueHtml clue.text)
        ]


clueElementId : ClueId -> String
clueElementId cid =
    "clue-" ++ String.fromInt cid.number ++ "-" ++ directionLabel cid.direction


{-| Render a clue text string that may contain <i>, <b>, or <span> tags.
-}
viewClueHtml : String -> List (Html msg)
viewClueHtml raw =
    raw
        |> String.split "<"
        |> List.indexedMap
            (\idx seg ->
                if idx == 0 then
                    if String.isEmpty seg then [] else [ text seg ]

                else if String.startsWith "/" seg then
                    -- Closing tag fragment: "/tagname>trailing"
                    case String.split ">" seg of
                        _ :: trailingParts ->
                            let
                                trailing =
                                    String.join ">" trailingParts
                            in
                            if String.isEmpty trailing then [] else [ text trailing ]

                        _ ->
                            []

                else
                    -- Opening tag fragment: "tagname>content"
                    case String.split ">" seg of
                        tagName :: contentParts ->
                            [ inlineTag tagName (String.join ">" contentParts) ]

                        _ ->
                            [ text ("<" ++ seg) ]
            )
        |> List.concat


inlineTag : String -> String -> Html msg
inlineTag tagName content =
    case tagName of
        "i" ->
            i [] [ text content ]

        "b" ->
            b [] [ text content ]

        _ ->
            span [] [ text content ]
