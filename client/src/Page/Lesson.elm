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
import Time exposing (Month(..), Posix)


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
            formatDayTime lesson.datetime
                ++ " - "
                ++ routineTopic
        ]


formatDayTime : Posix -> String
formatDayTime posix =
    let
        day =
            Time.toDay Time.utc posix

        month =
            toCzechMonth <| Time.toMonth Time.utc posix

        year =
            Time.toYear Time.utc posix
    in
    String.fromInt day ++ ". " ++ month ++ " " ++ String.fromInt year


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
