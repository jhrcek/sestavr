module Page.Lesson exposing
    ( Config
    , view
    )

import Color
import Common
import Dict.Any
import Domain exposing (Lesson, LessonId, LessonIdTag, Routine, RoutineIdTag)
import Element as E exposing (Element)
import Element.Border as Border
import Id exposing (IdDict)
import Router
import Time exposing (Month(..))
import Time.Extra as Time


type alias Config msg =
    { deleteLesson : LessonId -> msg
    }


view : Config msg -> IdDict LessonIdTag Lesson -> IdDict RoutineIdTag Routine -> Element msg
view config lessons routines =
    let
        cell =
            E.el
                [ Border.solid
                , Border.width 1
                , Border.color Color.lightGrey
                , E.height E.fill
                , E.padding 5
                ]
    in
    E.column []
        [ Common.heading1 "Lekce"
        , E.table
            [ Border.solid
            , Border.width 1
            , Border.color Color.lightGrey
            ]
            { data =
                Dict.Any.values lessons
                    |> List.sortBy (.datetime >> Time.posixToMillis)
                    |> List.reverse
            , columns =
                [ { header = cell <| E.el [ E.centerY, E.centerX ] <| E.text "Datum a čas"
                  , width = E.shrink
                  , view =
                        \lesson ->
                            cell <|
                                E.el [ E.alignRight ] <|
                                    E.text <|
                                        Time.formatDateTime lesson.datetime
                  }
                , { header = cell <| E.el [ E.centerY, E.centerX ] <| E.text "Sestava"
                  , width = E.fill
                  , view =
                        \lesson ->
                            let
                                routineTopic =
                                    Dict.Any.get lesson.routineId routines
                                        |> Maybe.map .topic
                                        |> Maybe.withDefault "Neznámá sestava"
                            in
                            cell <|
                                E.link (E.centerY :: Common.linkAttrs)
                                    { url = Router.href (Router.Routine lesson.routineId)
                                    , label = E.text routineTopic
                                    }
                  }
                , { header = cell (E.text " ")
                  , width = E.shrink
                  , view =
                        \lesson ->
                            E.el
                                [ Border.solid
                                , Border.width 1
                                , Border.color Color.lightGrey
                                ]
                                (Common.iconButton (config.deleteLesson lesson.id) "🗑")
                  }
                ]
            }
        ]
