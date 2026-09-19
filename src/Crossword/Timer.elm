module Crossword.Timer exposing
    ( Timer
    , elapsed
    , isRunning
    , pause
    , resume
    , tick
    , touch
    )

{-| How long a puzzle has taken, accumulated across visits.

The timer is never shown while solving — it records rather than pressures — so
it only has to be accurate, not live. It starts on the first letter entered,
pauses when the tab goes away, and stops counting once solving has been idle
for long enough that you have clearly wandered off.

-}

import Time


type Timer
    = Timer
        { accumulated : Int
        , running : Maybe Running
        }


type alias Running =
    { lastTick : Time.Posix
    , lastActivity : Time.Posix
    }


{-| Stop counting after this long without a keystroke. Time up to the cut-off
still counts: thinking is solving, staring at a different tab isn't.
-}
idleLimit : Int
idleLimit =
    5 * 60 * 1000


{-| Restore a timer holding time already banked in earlier visits.
-}
resume : Int -> Timer
resume accumulated =
    Timer { accumulated = accumulated, running = Nothing }


elapsed : Timer -> Int
elapsed (Timer t) =
    t.accumulated


isRunning : Timer -> Bool
isRunning (Timer t) =
    t.running /= Nothing


{-| Register solving activity, starting the clock if it wasn't running.
-}
touch : Time.Posix -> Timer -> Timer
touch now (Timer t) =
    case t.running of
        Nothing ->
            Timer { t | running = Just { lastTick = now, lastActivity = now } }

        Just running ->
            Timer { t | running = Just { running | lastActivity = now } }


{-| Bank the time since the last tick. Once the idle limit is passed the timer
banks only the part of the interval that fell before the cut-off and stops.
-}
tick : Time.Posix -> Timer -> Timer
tick now (Timer t) =
    case t.running of
        Nothing ->
            Timer t

        Just running ->
            let
                deadline =
                    Time.posixToMillis running.lastActivity + idleLimit

                since =
                    Time.posixToMillis running.lastTick
            in
            if Time.posixToMillis now >= deadline then
                Timer
                    { accumulated = t.accumulated + max 0 (deadline - since)
                    , running = Nothing
                    }

            else
                Timer
                    { accumulated = t.accumulated + max 0 (Time.posixToMillis now - since)
                    , running = Just { running | lastTick = now }
                    }


{-| Stop the clock without banking the part-second since the last tick. Used
when the tab is hidden; solving resumes the timer on the next keystroke.
-}
pause : Timer -> Timer
pause (Timer t) =
    Timer { t | running = Nothing }
