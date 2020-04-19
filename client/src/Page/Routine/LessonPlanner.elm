module Page.Routine.LessonPlanner exposing
    ( Config
    , LessonPlanner
    , Msg
    , init
    , update
    , view
    )

import Color
import Command
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
    , pickedTime : String
    }


type Msg
    = ScheduleLesson
    | NextMonth
    | PrevMonth
    | PickDay Int Month Int
    | SetTime String
    | CancelSchedulingLesson
    | SaveLesson


type alias Config msg =
    { validationError : String -> msg

    -- TODO change to Year, Month, Day , Hour and Minute or other more timey types
    , createLesson : ( Int, Month, Int ) -> ( Int, Int ) -> msg
    }


init : Posix -> LessonPlanner
init today =
    { today = today, picker = Nothing }


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

        PickDay year month day ->
            ( updatePlanner (\lp -> { lp | pickedDay = Just ( year, month, day ) }) model, Cmd.none )

        SetTime hhMmStr ->
            ( updatePlanner (\lp -> { lp | pickedTime = hhMmStr }) model, Cmd.none )

        SaveLesson ->
            case model.picker of
                Nothing ->
                    ( model, Cmd.none )

                Just picker ->
                    case saveLesson config picker of
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


saveLesson : Config msg -> DateTimePicker -> Result String msg
saveLesson config lessonPlanner =
    case lessonPlanner.pickedDay of
        Nothing ->
            Err "Musíš vybrat datum v kalendáři"

        Just yearMonthDay ->
            case parseHoursMinutes lessonPlanner.pickedTime of
                Err e ->
                    Err e

                Ok hourMinute ->
                    Ok <| config.createLesson yearMonthDay hourMinute


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

        Just dtp ->
            E.column []
                [ E.el [ Font.size 28, Font.bold ] (E.text "Plánování lekce")
                , E.text "Vyber datum a čas lekce"
                , datePicker dtp lessonPlanner.today
                , timePicker dtp
                , E.row [ E.spacing 5, E.paddingXY 0 5 ]
                    [ Input.button Common.buttonAttrs
                        { onPress = Just CancelSchedulingLesson
                        , label = E.text "Zrušit"
                        }
                    , Input.button Common.buttonAttrs
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
                , if String.isEmpty picker.pickedTime then
                    E.none

                  else
                    case parseHoursMinutes picker.pickedTime of
                        Ok _ ->
                            E.none

                        Err e ->
                            E.el [ E.centerY, Font.color Color.red ] <| E.text e
                ]

        -- If the date wasn't picked yet, don't show the time picker
        Nothing ->
            E.none


parseHoursMinutes : String -> Result String ( Int, Int )
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
                                        Ok ( h, m )

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
