module Router exposing
    ( Route(..)
    , RoutineEditorRoute(..)
    , href
    , parseUrl
    , toHash
    )

import Domain exposing (ExerciseId, RoutineId)
import Id
import Url exposing (Protocol(..), Url)
import Url.Parser as P exposing ((</>), Parser, int, s, top)


type Route
    = Home
    | Tags
    | Positions
    | Exercises
    | Exercise ExerciseId
    | ExerciseEditor (Maybe ExerciseId)
    | Routines
    | Routine RoutineId
    | RoutineEditor RoutineEditorRoute
    | Lessons
    | Images
    | NotFound String


type RoutineEditorRoute
    = NewRoutine
    | CopyRoutine RoutineId
    | EditRoutine RoutineId


route : Parser (Route -> a) a
route =
    P.oneOf
        [ P.map Home top
        , P.map Tags (s "tag")
        , P.map Positions (s "position")
        , P.map Lessons (s "lesson")
        , P.map Images (s "image")

        -- Exercises
        , P.map Exercises (s "exercise")
        , P.map (Exercise << Id.fromInt) (s "exercise" </> int)
        , P.map (ExerciseEditor Nothing) (s "exercise" </> s "create")
        , P.map (ExerciseEditor << Just << Id.fromInt) (s "exercise" </> int </> s "edit")

        -- Routines
        , P.map Routines (s "routine")
        , P.map (Routine << Id.fromInt) (s "routine" </> int)
        , P.map (RoutineEditor NewRoutine) (s "routine" </> s "create")
        , P.map (RoutineEditor << EditRoutine << Id.fromInt) (s "routine" </> int </> s "edit")
        , P.map (RoutineEditor << CopyRoutine << Id.fromInt) (s "routine" </> int </> s "copy")
        ]


parseUrl : Url -> Route
parseUrl url =
    let
        fragment =
            Maybe.withDefault "" url.fragment
    in
    P.parse route { url | path = fragment, fragment = Nothing }
        |> Maybe.withDefault (NotFound fragment)


toHash : Route -> String
toHash r =
    case r of
        Home ->
            "/"

        Tags ->
            "/tag"

        Positions ->
            "/position"

        Lessons ->
            "/lesson"

        Exercises ->
            "/exercise"

        Exercise exerciseId ->
            "/exercise/" ++ Id.toString exerciseId

        ExerciseEditor maybeExerciseId ->
            case maybeExerciseId of
                Nothing ->
                    "/exercise/create"

                Just exerciseId ->
                    "/exercise/" ++ Id.toString exerciseId ++ "/edit"

        Routines ->
            "/routine"

        Routine routineId ->
            "/routine/" ++ Id.toString routineId

        RoutineEditor routineEditorRoute ->
            case routineEditorRoute of
                NewRoutine ->
                    "/routine/create"

                EditRoutine routineId ->
                    "/routine/" ++ Id.toString routineId ++ "/edit"

                CopyRoutine routineId ->
                    "/routine/" ++ Id.toString routineId ++ "/copy"

        Images ->
            "/image"

        NotFound bad ->
            bad


href : Route -> String
href r =
    "#" ++ toHash r
