module Page.Lesson exposing
    ( Config
    , view
    )

import Common
import Dict.Any
import Domain exposing (Lesson, LessonId, LessonIdTag, Routine, RoutineIdTag)
import Element as E exposing (Element)
import Element.Border as Border
import Element.Input as Input
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
        cellSize =
            33

        cellAttrs =
            [ Border.solid
            , Border.width 1
            , E.height (E.px cellSize)
            , E.centerX
            , E.centerY
            ]
    in
    E.column []
        [ Common.heading1 "Lekce"
        , E.table
            [ Border.solid
            , Border.width 1
            ]
            { data =
                Dict.Any.values lessons
                    |> List.sortBy (.datetime >> Time.posixToMillis)
                    |> List.reverse
            , columns =
                [ { header =
                        E.el cellAttrs (E.text "Datum a čas")
                  , width = E.px 205
                  , view =
                        \lesson ->
                            E.el cellAttrs <|
                                E.el [ E.centerY ] <|
                                    E.text <|
                                        Time.formatDateTime lesson.datetime
                  }
                , { header = E.el cellAttrs <| E.text "Sestava"
                  , width = E.fill
                  , view =
                        \lesson ->
                            let
                                routineTopic =
                                    Dict.Any.get lesson.routineId routines
                                        |> Maybe.map .topic
                                        |> Maybe.withDefault "Neznámá sestava"
                            in
                            E.el cellAttrs <|
                                E.link (E.centerY :: Common.linkAttrs)
                                    { url = Router.href (Router.Routine lesson.routineId)
                                    , label = E.text routineTopic
                                    }
                  }
                , { header = E.el cellAttrs <| E.text "Možnosti"
                  , width = E.px 95
                  , view =
                        \lesson ->
                            E.el
                                [ Border.solid
                                , Border.width 1
                                , E.centerX
                                , E.centerY
                                , E.height (E.px cellSize)
                                , E.width (E.px cellSize)
                                ]
                            <|
                                Input.button Common.buttonAttrs
                                    { onPress = Just <| config.deleteLesson lesson.id
                                    , label = E.text "🗑"
                                    }
                  }
                ]
            }
        ]
