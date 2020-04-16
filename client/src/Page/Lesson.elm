module Page.Lesson exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Dict.Any
import Domain exposing (Lesson, LessonIdTag, Routine, RoutineIdTag)
import Element as E exposing (Element)
import Id exposing (IdDict)
import Time exposing (Month(..))
import Time.Extra as Time


type alias Model =
    {}


type Msg
    = Msg


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg ->
            model


view : IdDict LessonIdTag Lesson -> IdDict RoutineIdTag Routine -> Model -> Element msg
view lessons routines model =
    Dict.Any.values lessons
        |> List.sortBy (.datetime >> Time.posixToMillis)
        |> List.reverse
        |> List.map (lessonView routines)
        |> E.column []


lessonView : IdDict RoutineIdTag Routine -> Lesson -> Element msg
lessonView routines lesson =
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
                ++ routineTopic
        ]
