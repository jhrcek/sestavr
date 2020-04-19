module Page.Routine.LessonPlanner exposing
    ( LessonPlanner
    , Msg
    , init
    , update
    , view
    )

import Color
import Common
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import List.Extra as List
import Time exposing (Month(..), Posix, Weekday(..))
import Time.Extra as Time


type alias LessonPlanner =
    { today : Posix
    , picker : Maybe DateTimePicker
    }


type alias DateTimePicker =
    { pickerYear : Int
    , pickerMonth : Month
    , pickedDay : Maybe ( Int, Month, Int )
    }


type Msg
    = ScheduleLesson
    | NextMonth
    | PrevMonth
    | PickDay Int Month Int
    | CancelSchedulingLesson
    | SaveLesson


init : Posix -> LessonPlanner
init today =
    { today = today, picker = Nothing }


update : Msg -> LessonPlanner -> LessonPlanner
update msg model =
    case msg of
        ScheduleLesson ->
            { model
                | picker =
                    Just
                        { pickerYear = Time.toYear Time.utc model.today
                        , pickerMonth = Time.toMonth Time.utc model.today
                        , pickedDay = Nothing
                        }
            }

        CancelSchedulingLesson ->
            { model | picker = Nothing }

        SaveLesson ->
            -- TODO fire save command or display error modal here
            { model | picker = Nothing }

        NextMonth ->
            switchMonth Time.nextMonthYear model

        PrevMonth ->
            switchMonth Time.prevMonthYear model

        PickDay year month day ->
            updatePlanner
                (\lp -> { lp | pickedDay = Just ( year, month, day ) })
                model


updatePlanner : (DateTimePicker -> DateTimePicker) -> LessonPlanner -> LessonPlanner
updatePlanner f p =
    { p | picker = Maybe.map f p.picker }


switchMonth : (Month -> Int -> ( Month, Int )) -> LessonPlanner -> LessonPlanner
switchMonth monthSwitcher =
    updatePlanner
        (\lp ->
            let
                ( newMonth, newYear ) =
                    monthSwitcher lp.pickerMonth lp.pickerYear
            in
            { lp
                | pickerYear = newYear
                , pickerMonth = newMonth
                , pickedDay = Nothing
            }
        )


view : LessonPlanner -> Element Msg
view lessonPlanner =
    case lessonPlanner.picker of
        Nothing ->
            Input.button Common.buttonAttrs
                { onPress = Just ScheduleLesson
                , label = E.text "Naplánovat lekci"
                }

        Just rec ->
            E.column []
                [ E.el [ Font.size 28, Font.bold ] (E.text "Plánování lekce")
                , datePicker rec lessonPlanner.today
                , E.row [ E.spacing 5, E.padding 5 ]
                    [ Input.button Common.buttonAttrs
                        { onPress = Just CancelSchedulingLesson
                        , label = E.text "Zrušit"
                        }
                    , Input.button Common.buttonAttrs
                        { onPress = Nothing -- TODO SaveLesson
                        , label = E.text "Vytvořit lekci"
                        }
                    ]
                ]


datePicker : DateTimePicker -> Posix -> Element Msg
datePicker { pickerYear, pickerMonth, pickedDay } today =
    let
        isPickedDay y m d =
            Maybe.withDefault False <|
                Maybe.map (\( py, pm, pd ) -> y == py && m == pm && d == pd) pickedDay

        cellSize =
            45

        cellPadding =
            11

        firstDay =
            Time.firstDayOfMonthWeekday pickerYear pickerMonth

        firstDayOffset =
            Time.weekDayOffset firstDay

        daysInMonth =
            Time.daysInMonth pickerYear pickerMonth

        weeks =
            List.map
                (\w ->
                    -- Pad last week if it doesn't end with sunday
                    if List.length w /= 7 then
                        w ++ List.repeat (7 - List.length w) 0

                    else
                        w
                )
            <|
                List.greedyGroupsOf 7 <|
                    -- Pad the first week with empty cells if it doesn't start on monday
                    List.repeat firstDayOffset 0
                        ++ List.range 1 daysInMonth
    in
    E.column [ Border.solid, Border.width 1 ] <|
        E.row [ E.width E.fill, E.height <| E.px cellSize, E.padding 5 ]
            [ E.el [ Event.onClick PrevMonth ] (E.text "«")
            , E.el [ E.centerX ] (E.text <| Time.toCzechMonth pickerMonth ++ " " ++ String.fromInt pickerYear)
            , E.el [ Event.onClick NextMonth, E.alignRight ] (E.text "»")
            ]
            :: E.row []
                (List.map
                    (\wd ->
                        E.el
                            [ E.width <| E.px cellSize
                            , E.height <| E.px cellSize
                            , Border.solid
                            , Border.width 1
                            , E.padding cellPadding
                            ]
                        <|
                            E.text <|
                                Time.weekDay2Letters wd
                    )
                    Time.weekDays
                )
            :: List.map
                (\week ->
                    E.row [] <|
                        List.map
                            (\dayNumber ->
                                let
                                    isToday =
                                        (dayNumber == Time.toDay Time.utc today)
                                            && (pickerMonth == Time.toMonth Time.utc today)
                                            && (pickerYear == Time.toYear Time.utc today)

                                    isPicked =
                                        isPickedDay pickerYear pickerMonth dayNumber
                                in
                                E.el
                                    [ E.width <| E.px cellSize
                                    , E.height <| E.px cellSize
                                    , Border.dotted
                                    , Border.width 1
                                    ]
                                <|
                                    if dayNumber /= 0 then
                                        E.el
                                            [ Event.onClick <| PickDay pickerYear pickerMonth dayNumber
                                            , E.width <| E.px <| cellSize - 2
                                            , E.height <| E.px <| cellSize - 2
                                            , E.padding cellPadding
                                            , Font.center
                                            , Border.rounded <| cellSize // 2
                                            , Background.color <|
                                                if isPicked then
                                                    Color.darkBlue

                                                else if isToday then
                                                    Color.lightGrey

                                                else
                                                    Color.white
                                            , Font.color <|
                                                if isPicked then
                                                    Color.white

                                                else
                                                    Color.black
                                            ]
                                        <|
                                            E.text <|
                                                String.fromInt dayNumber

                                    else
                                        E.none
                            )
                            week
                )
                weeks
