module Page.Routine.LessonPlanner exposing
    ( Config
    , LessonPlanner
    , Msg
    , init
    , update
    , view
    )

import Calendar
import Clock
import Color
import Command
import Common
import DateTime
import Domain exposing (Lesson, RoutineId)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Id
import List.Extra as List
import Time exposing (Month, Posix)
import Time.Extra as Time


type alias LessonPlanner =
    { today : Posix
    , routineId : RoutineId
    , picker : Maybe DateTimePicker
    }


type alias DateTimePicker =
    { pickerYear : Int
    , pickerMonth : Month
    , pickedDay : Maybe Int
    , pickedTime : String
    }


type Msg
    = ScheduleLesson
    | NextMonth
    | PrevMonth
    | PickDay Int
    | SetTime String
    | CancelSchedulingLesson
    | SaveLesson


type alias Config msg =
    { validationError : String -> msg
    , createLesson : Lesson -> msg
    }


init : Posix -> RoutineId -> LessonPlanner
init today routineId =
    { today = today
    , picker = Nothing
    , routineId = routineId
    }


update : Config msg -> Msg -> LessonPlanner -> ( LessonPlanner, Cmd msg )
update config msg model =
    case msg of
        ScheduleLesson ->
            ( { model
                | picker =
                    Just
                        { pickerYear = Time.toYear Time.utc model.today
                        , pickerMonth = Time.toMonth Time.utc model.today
                        , pickedDay = Nothing
                        , pickedTime = ""
                        }
              }
            , Cmd.none
            )

        CancelSchedulingLesson ->
            ( { model | picker = Nothing }, Cmd.none )

        NextMonth ->
            ( switchMonth Time.nextMonthYear model, Cmd.none )

        PrevMonth ->
            ( switchMonth Time.prevMonthYear model, Cmd.none )

        PickDay day ->
            ( updatePlanner (\lp -> { lp | pickedDay = Just day }) model, Cmd.none )

        SetTime hhMmStr ->
            ( updatePlanner (\lp -> { lp | pickedTime = hhMmStr }) model, Cmd.none )

        SaveLesson ->
            case model.picker of
                Nothing ->
                    ( model, Cmd.none )

                Just picker ->
                    case saveLesson config picker model.routineId of
                        Err validationError ->
                            ( model
                            , Command.perform <| config.validationError validationError
                            )

                        Ok saveCommand ->
                            ( { model | picker = Nothing }
                            , Command.perform saveCommand
                            )


updatePlanner : (DateTimePicker -> DateTimePicker) -> LessonPlanner -> LessonPlanner
updatePlanner f p =
    { p | picker = Maybe.map f p.picker }


saveLesson : Config msg -> DateTimePicker -> RoutineId -> Result String msg
saveLesson config picker routineId =
    case picker.pickedDay of
        Nothing ->
            Err "Musíš vybrat datum v kalendáři"

        Just pickedDay ->
            case Calendar.fromRawParts { day = pickedDay, month = picker.pickerMonth, year = picker.pickerYear } of
                Nothing ->
                    Err "Tohle datum není v kalendáři"

                Just date ->
                    parseHoursMinutes picker.pickedTime
                        |> Result.map
                            (\time ->
                                let
                                    newLesson =
                                        { id = Id.fromInt -1
                                        , routineId = routineId
                                        , datetime = DateTime.toPosix <| DateTime.fromDateAndTime date time
                                        }
                                in
                                config.createLesson newLesson
                            )


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
            Input.button Common.blueButton
                { onPress = Just ScheduleLesson
                , label = E.text "Naplánovat lekci"
                }

        Just dtp ->
            E.column []
                [ Common.heading1 "Plánování lekce"
                , E.text "Vyber datum a čas lekce"
                , datePicker dtp lessonPlanner.today
                , if isPastDayPicked dtp lessonPlanner.today then
                    E.el [ Font.color Color.orange ]
                        (E.text "Zvolené datum je v minulosti (může být, pokud to chceš)")

                  else
                    E.none
                , timePicker dtp
                , E.row [ E.spacing 5, E.paddingXY 0 5 ]
                    [ Input.button Common.coralButton
                        { onPress = Just CancelSchedulingLesson
                        , label = E.text "Zrušit"
                        }
                    , Input.button Common.blueButton
                        { onPress = Just SaveLesson
                        , label = E.text "Vytvořit lekci"
                        }
                    ]
                ]


timePicker : DateTimePicker -> Element Msg
timePicker picker =
    case picker.pickedDay of
        Just _ ->
            E.row []
                [ Input.text [ E.padding 4, E.width (E.px 100) ]
                    { onChange = SetTime
                    , text = picker.pickedTime
                    , placeholder = Just <| Input.placeholder [] <| E.text "HH:MM"
                    , label = Input.labelLeft [ E.centerY ] (E.text "Čas lekce")
                    }
                ]

        -- If the date wasn't picked yet, don't show the time picker
        Nothing ->
            E.none


parseHoursMinutes : String -> Result String Clock.Time
parseHoursMinutes str =
    if String.isEmpty str then
        Err "Musíš zvolit datum a čas"

    else
        case String.split ":" str of
            [ hh, mm ] ->
                case String.toInt hh of
                    Just h ->
                        if 0 <= h && h < 24 then
                            case String.toInt mm of
                                Just m ->
                                    if 0 <= m && m <= 59 then
                                        case
                                            Clock.fromRawParts
                                                { hours = h
                                                , minutes = m
                                                , seconds = 0
                                                , milliseconds = 0
                                                }
                                        of
                                            Just time ->
                                                Ok time

                                            Nothing ->
                                                -- Should never happen, as we already validated hours and minutes
                                                Err <| "Nepodařilo se sestavit čas z hodin a minut: " ++ String.fromInt h ++ ":" ++ String.fromInt m

                                    else
                                        Err "Minuta musí být v rozmezí 0 - 59"

                                Nothing ->
                                    Err "Minuta musí být číslo"

                        else
                            Err "Hodina musí být v rozmezí 0 - 23"

                    Nothing ->
                        Err "Hodina musí být číslo"

            _ ->
                Err "Čas musí mít format HH:MM"


isPastDayPicked : DateTimePicker -> Posix -> Bool
isPastDayPicked { pickerYear, pickerMonth, pickedDay } today =
    pickedDay
        |> Maybe.andThen (\pd -> Calendar.fromRawParts { year = pickerYear, month = pickerMonth, day = pd })
        |> Maybe.map (\pickedDate -> Calendar.compare pickedDate (Calendar.fromPosix today) == LT)
        |> Maybe.withDefault False


datePicker : DateTimePicker -> Posix -> Element Msg
datePicker { pickerYear, pickerMonth, pickedDay } today =
    let
        isPickedDay dayNumber =
            Maybe.withDefault False <|
                Maybe.map (\pd -> dayNumber == pd)
                    pickedDay

        isToday dayNumber =
            (dayNumber == Time.toDay Time.utc today)
                && (pickerMonth == Time.toMonth Time.utc today)
                && (pickerYear == Time.toYear Time.utc today)

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
    E.column [ Border.width 1, Border.solid ] <|
        E.row [ E.width E.fill, E.height <| E.px cellSize, E.padding 5, Border.width 1 ]
            [ E.el [ Event.onClick PrevMonth ] (E.text "◀")
            , E.el [ E.centerX ] (E.text <| Time.toCzechMonth pickerMonth ++ " " ++ String.fromInt pickerYear)
            , E.el [ Event.onClick NextMonth, E.alignRight ] (E.text "▶")
            ]
            :: E.row []
                (List.map
                    (\wd ->
                        E.el
                            [ E.width <| E.px cellSize
                            , E.height <| E.px cellSize
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
                                E.el
                                    [ E.width <| E.px cellSize
                                    , E.height <| E.px cellSize
                                    , Border.dotted
                                    , Border.width 1
                                    ]
                                <|
                                    if dayNumber /= 0 then
                                        E.el
                                            [ Event.onClick <| PickDay dayNumber
                                            , E.width <| E.px <| cellSize - 2
                                            , E.height <| E.px <| cellSize - 2
                                            , E.padding cellPadding
                                            , Font.center
                                            , Border.rounded <| cellSize // 2
                                            , Background.color <|
                                                if isPickedDay dayNumber then
                                                    Color.darkBlue

                                                else if isToday dayNumber then
                                                    Color.lightGrey

                                                else
                                                    Color.white
                                            , Font.color <|
                                                if isPickedDay dayNumber then
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
