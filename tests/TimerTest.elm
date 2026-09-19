module TimerTest exposing (suite)

import Crossword.Timer as Timer
import Expect
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Crossword.Timer"
        [ test "a fresh timer is stopped and holds only banked time" <|
            \_ ->
                Timer.resume 5000
                    |> Expect.all
                        [ Timer.elapsed >> Expect.equal 5000
                        , Timer.isRunning >> Expect.equal False
                        ]
        , test "ticks before any activity bank nothing" <|
            \_ ->
                Timer.resume 0
                    |> Timer.tick (at 1000)
                    |> Timer.elapsed
                    |> Expect.equal 0
        , test "activity starts the clock and ticks bank the gap" <|
            \_ ->
                Timer.resume 0
                    |> Timer.touch (at 0)
                    |> Timer.tick (at 1000)
                    |> Timer.tick (at 2500)
                    |> Timer.elapsed
                    |> Expect.equal 2500
        , test "time already banked is added to" <|
            \_ ->
                Timer.resume 90000
                    |> Timer.touch (at 0)
                    |> Timer.tick (at 1000)
                    |> Timer.elapsed
                    |> Expect.equal 91000
        , test "thinking counts up to the idle limit" <|
            \_ ->
                -- four minutes of silence is still solving
                Timer.resume 0
                    |> Timer.touch (at 0)
                    |> Timer.tick (at (4 * minute))
                    |> Expect.all
                        [ Timer.elapsed >> Expect.equal (4 * minute)
                        , Timer.isRunning >> Expect.equal True
                        ]
        , test "idling past the limit banks only the time up to the cut-off" <|
            \_ ->
                Timer.resume 0
                    |> Timer.touch (at 0)
                    |> Timer.tick (at (20 * minute))
                    |> Expect.all
                        [ Timer.elapsed >> Expect.equal (5 * minute)
                        , Timer.isRunning >> Expect.equal False
                        ]
        , test "activity after idling restarts the clock without back-filling" <|
            \_ ->
                Timer.resume 0
                    |> Timer.touch (at 0)
                    |> Timer.tick (at (20 * minute))
                    |> Timer.touch (at (21 * minute))
                    |> Timer.tick (at (21 * minute + 1000))
                    |> Timer.elapsed
                    |> Expect.equal (5 * minute + 1000)
        , test "pausing stops the clock and later ticks bank nothing" <|
            \_ ->
                Timer.resume 0
                    |> Timer.touch (at 0)
                    |> Timer.tick (at 1000)
                    |> Timer.pause
                    |> Timer.tick (at 60000)
                    |> Timer.elapsed
                    |> Expect.equal 1000
        ]


at : Int -> Time.Posix
at =
    Time.millisToPosix


minute : Int
minute =
    60 * 1000
