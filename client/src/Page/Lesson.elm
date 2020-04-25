module Page.Lesson exposing
    ( Config
    , view
    )

import Common
import Dict.Any
import Domain exposing (Lesson, LessonId, LessonIdTag, Routine, RoutineIdTag)
import Element as E exposing (Element)
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
    Dict.Any.values lessons
        |> List.sortBy (.datetime >> Time.posixToMillis)
        |> List.reverse
        |> List.map (lessonView config routines)
        -- TODO there should probably be "Vytvořit lekci" button here
        |> (::) (Common.heading1 "Lekce")
        |> E.column [ E.spacing 5 ]


lessonView : Config msg -> IdDict RoutineIdTag Routine -> Lesson -> Element msg
lessonView config routines lesson =
    let
        routineTopic =
            Dict.Any.get lesson.routineId routines
                |> Maybe.map .topic
                |> Maybe.withDefault "Neznámá sestava"
    in
    E.row []
        [ E.text <|
            Time.formatDateTime lesson.datetime
                ++ " - "
        , E.el [ E.width <| E.px 400 ] <|
            E.link Common.linkAttrs
                { url = Router.href (Router.Routine lesson.routineId)
                , label = E.text routineTopic
                }
        , Input.button Common.buttonAttrs
            { onPress = Just <| config.deleteLesson lesson.id
            , label = E.text "Odstranit"
            }
        ]
