module Time.Extra exposing
    ( daysInMonth
    , firstDayOfMonthWeekday
    , formatPosix
    , nextMonthYear
    , prevMonthYear
    , toCzechMonth
    , weekDay2Letters
    , weekDayOffset
    , weekDays
    )

import Array
import Calendar
import Time exposing (Month(..), Posix, Weekday(..))


toCzechMonth : Month -> String
toCzechMonth month =
    case month of
        Jan ->
            "Leden"

        Feb ->
            "Únor"

        Mar ->
            "Březen"

        Apr ->
            "Duben"

        May ->
            "Květen"

        Jun ->
            "Červen"

        Jul ->
            "Červenec"

        Aug ->
            "Srpen"

        Sep ->
            "Září"

        Oct ->
            "Říjen"

        Nov ->
            "Listopad"

        Dec ->
            "Prosinec"


nextMonthYear : Month -> Int -> ( Month, Int )
nextMonthYear month year =
    ( Calendar.months
        |> Array.get (Calendar.monthToInt month |> modBy 12)
        |> Maybe.withDefault Jan
    , if month == Dec then
        year + 1

      else
        year
    )


prevMonthYear : Month -> Int -> ( Month, Int )
prevMonthYear month year =
    ( Calendar.months
        |> Array.get (Calendar.monthToInt month - 2 |> modBy 12)
        |> Maybe.withDefault Jan
    , if month == Jan then
        year - 1

      else
        year
    )


formatPosix : Posix -> String
formatPosix posix =
    let
        day =
            Time.toDay Time.utc posix

        month =
            toCzechMonth <| Time.toMonth Time.utc posix

        year =
            Time.toYear Time.utc posix

        hour =
            Time.toHour Time.utc posix

        minute =
            Time.toMinute Time.utc posix
    in
    String.fromInt day
        ++ ". "
        ++ month
        ++ " "
        ++ String.fromInt year
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt hour)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt minute)


weekDays : List Weekday
weekDays =
    [ Mon
    , Tue
    , Wed
    , Thu
    , Fri
    , Sat
    , Sun
    ]


weekDay2Letters : Weekday -> String
weekDay2Letters weekday =
    case weekday of
        Mon ->
            "Po"

        Tue ->
            "Út"

        Wed ->
            "St"

        Thu ->
            "Čt"

        Fri ->
            "Pá"

        Sat ->
            "So"

        Sun ->
            "Ne"


firstDayOfMonthWeekday : Int -> Month -> Weekday
firstDayOfMonthWeekday year month =
    case Calendar.fromRawParts { year = year, month = month, day = 1 } of
        Just firstDayOfMonth ->
            Calendar.getWeekday firstDayOfMonth

        -- Should not happen because year is positive and each month contains day 1
        Nothing ->
            Mon


daysInMonth : Int -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if
                (modBy 4 year == 0)
                    && (modBy 100 year /= 0)
                    && (modBy 400 year == 0)
            then
                29

            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


weekDayOffset : Weekday -> Int
weekDayOffset weekday =
    case weekday of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6
