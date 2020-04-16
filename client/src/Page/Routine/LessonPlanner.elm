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
import Html
import Html.Attributes as Attr
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
    }


type Msg
    = ScheduleLesson
    | NextMonth
    | PrevMonth
    | CancelSchedulingLesson
    | SaveLesson


type alias DateTime =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    }


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
                        }
            }

        CancelSchedulingLesson ->
            { model | picker = Nothing }

        SaveLesson ->
            -- TODO fire save command or display error modal here
            { model | picker = Nothing }

        NextMonth ->
            updatePlanner
                (\lp ->
                    let
                        ( newMonth, newYear ) =
                            Time.nextMonthYear lp.pickerMonth lp.pickerYear
                    in
                    { lp
                        | pickerYear = newYear
                        , pickerMonth = newMonth
                    }
                )
                model

        PrevMonth ->
            updatePlanner
                (\lp ->
                    let
                        ( newMonth, newYear ) =
                            Time.prevMonthYear lp.pickerMonth lp.pickerYear
                    in
                    { lp
                        | pickerYear = newYear
                        , pickerMonth = newMonth
                    }
                )
                model


updatePlanner : (DateTimePicker -> DateTimePicker) -> LessonPlanner -> LessonPlanner
updatePlanner f p =
    { p | picker = Maybe.map f p.picker }


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
                , datePicker rec.pickerYear rec.pickerMonth lessonPlanner.today
                , E.row []
                    [ E.html <| Html.input [ Attr.type_ "date" ] []
                    , E.html <|
                        Html.input
                            [ Attr.type_ "time"
                            , Attr.step "60" -- minute precision, don't wanna seconds input
                            ]
                            []
                    ]
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


datePicker : Int -> Month -> Posix -> Element Msg
datePicker year month today =
    let
        -- TODO highlight today
        cellSize =
            45

        cellPadding =
            10

        firstDay =
            Time.firstDayOfMonthWeekday year month

        firstDayOffset =
            Time.weekDayOffset firstDay

        daysInMonth =
            Time.daysInMonth year month

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
            , E.el [ E.centerX ] (E.text <| Time.toCzechMonth month ++ " " ++ String.fromInt year)
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
                            , Font.center
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
                                    ((if
                                        dayNumber
                                            == Time.toDay Time.utc today
                                            && year
                                            == Time.toYear Time.utc today
                                            && month
                                            == Time.toMonth Time.utc today
                                      then
                                        (::) (Background.color Color.lightGrey)

                                      else
                                        identity
                                     )
                                        [ E.width <| E.px cellSize
                                        , E.height <| E.px cellSize
                                        , Font.center
                                        , E.padding cellPadding
                                        , Border.dotted
                                        , Border.width 1
                                        ]
                                    )
                                <|
                                    if dayNumber /= 0 then
                                        E.text <| String.fromInt dayNumber

                                    else
                                        E.none
                            )
                            week
                )
                weeks
