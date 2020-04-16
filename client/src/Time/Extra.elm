module Time.Extra exposing
    ( daysInMonth
    , firstDayOfMonthWeekday
    , formatDateTime
    , monthNumber
    , nextMonthYear
    , prevMonthYear
    , toCzechMonth
    , weekDay2Letters
    , weekDayOffset
    , weekDays
    )

import Iso8601
import List.Extra as List
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
    ( List.getAt (monthNumber month |> modBy 12) months
        |> Maybe.withDefault Jan
    , if month == Dec then
        year + 1

      else
        year
    )


prevMonthYear : Month -> Int -> ( Month, Int )
prevMonthYear month year =
    ( List.getAt (monthNumber month - 2 |> modBy 12) months
        |> Maybe.withDefault Jan
    , if month == Jan then
        year - 1

      else
        year
    )


formatDateTime : Posix -> String
formatDateTime posix =
    let
        day =
            Time.toDay Time.utc posix

        month =
            toCzechMonth <| Time.toMonth Time.utc posix

        year =
            Time.toYear Time.utc posix
    in
    String.fromInt day ++ ". " ++ month ++ " " ++ String.fromInt year


monthNumber : Month -> Int
monthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


months : List Month
months =
    [ Jan
    , Feb
    , Mar
    , Apr
    , May
    , Jun
    , Jul
    , Aug
    , Sep
    , Oct
    , Nov
    , Dec
    ]


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
    let
        monthNumStr =
            String.padLeft 2 '0' <| String.fromInt <| monthNumber month
    in
    (String.fromInt year ++ "-" ++ monthNumStr ++ "-01")
        |> Iso8601.toTime
        |> Result.map (Time.toWeekday Time.utc)
        -- TODO this is really hacky way of getting weekday of first day of month.
        -- Find some library that makes this easier and without returning errors
        -- Just use https://package.elm-lang.org/packages/justinmimbs/date/
        |> Result.withDefault Mon


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
