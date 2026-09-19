module Crossword.View.Landing exposing (view)

import Crossword.History as History exposing (Entry, Tally)
import Crossword.Types exposing (LandingModel, LoadState(..), Msg(..))
import Html exposing (Html, button, div, form, h1, h2, input, label, li, ol, p, span, text)
import Html.Attributes as Attr
import Html.Events
import Time


view : LandingModel -> Html Msg
view model =
    div [ Attr.class "landing" ]
        [ h1 [ Attr.class "landing__title" ] [ text "Crosswords" ]
        , viewLoader model
        , viewStats model.history
        , viewHistory model.history
        , viewCredit
        ]


viewCredit : Html Msg
viewCredit =
    Html.footer [ Attr.class "landing__credit" ]
        [ p [ Attr.class "landing__byline" ]
            [ text "Made by "
            , link "https://whitebeard.blog" "Whitebeard"
            ]
        , p []
            [ text "The anagram finder uses the "
            , link "https://github.com/rdeits/cryptics/blob/master/raw_data/UKACD.txt"
                "UK Advanced Cryptics Dictionary"
            , text " by J Ross Beresford, used under a BSD-style licence."
            ]
        ]


link : String -> String -> Html Msg
link href label =
    Html.a
        [ Attr.href href
        , Attr.target "_blank"
        , Attr.rel "noopener"
        ]
        [ text label ]



-- LOADING A NEW PUZZLE


viewLoader : LandingModel -> Html Msg
viewLoader model =
    div [ Attr.class "landing__loader" ]
        [ form
            [ Attr.class "landing__form"
            , Html.Events.onSubmit LoadRequested
            ]
            [ label [ Attr.class "landing__label", Attr.for "crossword-path" ]
                [ text "Guardian crossword path" ]
            , div [ Attr.class "landing__form-row" ]
                [ input
                    [ Attr.class "landing__input"
                    , Attr.id "crossword-path"
                    , Attr.type_ "text"
                    , Attr.placeholder "cryptic/29963"
                    , Attr.value model.path
                    , Attr.autofocus True
                    , Html.Events.onInput PathChanged
                    ]
                    []
                , button
                    [ Attr.class "landing__submit"
                    , Attr.disabled (model.load == Loading || String.isEmpty (String.trim model.path))
                    ]
                    [ text
                        (if model.load == Loading then
                            "Loading…"

                         else
                            "Load"
                        )
                    ]
                ]
            ]
        , case model.load of
            LoadFailed err ->
                p [ Attr.class "landing__error" ] [ text ("Couldn't load that crossword: " ++ err) ]

            _ ->
                text ""
        ]



-- STATISTICS


viewStats : List Entry -> Html Msg
viewStats history =
    if List.isEmpty history then
        text ""

    else
        div [ Attr.class "landing__stats" ]
            [ viewTallyTable "By series" (History.seriesTallies history)
            , viewTallyTable "By publication day"
                (History.weekdayTallies history
                    |> List.filter (\( _, tally ) -> tally.started > 0)
                    |> List.map (\( day, tally ) -> ( weekdayName day, tally ))
                )
            ]


viewTallyTable : String -> List ( String, Tally ) -> Html Msg
viewTallyTable heading rows =
    div [ Attr.class "landing__stat-block" ]
        (h2 [ Attr.class "landing__stat-heading" ] [ text heading ]
            :: List.map viewTallyRow rows
        )


viewTallyRow : ( String, Tally ) -> Html Msg
viewTallyRow ( name, tally ) =
    div [ Attr.class "landing__stat-row" ]
        [ span [ Attr.class "landing__stat-name" ] [ text name ]
        , span [ Attr.class "landing__stat-bar" ]
            [ span
                [ Attr.class "landing__stat-fill"
                , Attr.style "width" (percentage tally ++ "%")
                ]
                []
            ]
        , span [ Attr.class "landing__stat-value" ]
            [ text (String.fromInt tally.completed ++ "/" ++ String.fromInt tally.started)
            , span [ Attr.class "landing__stat-time" ] [ text (averageNote tally) ]
            ]
        ]


percentage : Tally -> String
percentage tally =
    if tally.started == 0 then
        "0"

    else
        String.fromInt (round (100 * toFloat tally.completed / toFloat tally.started))


{-| Average time over the puzzles that were finished — an average including
abandoned ones would say more about giving up than about solving.
-}
averageNote : Tally -> String
averageNote tally =
    if tally.completed == 0 then
        ""

    else
        " · " ++ duration (tally.totalElapsed // tally.completed) ++ " avg"



-- HISTORY LIST


viewHistory : List Entry -> Html Msg
viewHistory history =
    if List.isEmpty history then
        p [ Attr.class "landing__empty" ]
            [ text "No crosswords yet. Load one above and it'll be listed here." ]

    else
        div [ Attr.class "landing__history" ]
            (History.grouped history |> List.map viewSeries)


viewSeries : ( String, List Entry ) -> Html Msg
viewSeries ( series, entries ) =
    div [ Attr.class "landing__series" ]
        [ h2 [ Attr.class "landing__series-name" ] [ text series ]
        , ol [ Attr.class "landing__entries" ] (List.map viewEntry entries)
        ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    li [ Attr.class "landing__entry" ]
        [ button
            [ Attr.class "landing__entry-button"
            , Attr.classList [ ( "landing__entry-button--done", History.completed entry ) ]
            , Attr.type_ "button"
            , Html.Events.onClick (HistoryEntryClicked entry.path)
            ]
            [ span [ Attr.class "landing__entry-number" ] [ text (String.fromInt entry.number) ]
            , span [ Attr.class "landing__entry-name" ] [ text (entryName entry) ]
            , span [ Attr.class "landing__entry-state" ] [ text (progressNote entry) ]
            ]
        ]


entryName : Entry -> String
entryName entry =
    case entry.setter of
        Just setter ->
            "by " ++ setter

        Nothing ->
            entry.name


progressNote : Entry -> String
progressNote entry =
    if History.completed entry then
        "solved · " ++ duration entry.elapsed

    else if entry.filled == 0 then
        "not started"

    else
        String.fromInt entry.filled
            ++ "/"
            ++ String.fromInt entry.cells
            ++ " · "
            ++ duration entry.elapsed



-- FORMATTING


duration : Int -> String
duration millis =
    let
        total =
            millis // 1000

        hours =
            total // 3600

        minutes =
            remainderBy 60 (total // 60)

        seconds =
            remainderBy 60 total
    in
    if hours > 0 then
        String.fromInt hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds

    else
        String.fromInt minutes ++ ":" ++ pad seconds


pad : Int -> String
pad n =
    String.padLeft 2 '0' (String.fromInt n)


weekdayName : Time.Weekday -> String
weekdayName day =
    case day of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"
