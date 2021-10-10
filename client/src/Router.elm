module Router exposing
    ( Route(..)
    , RoutineEditorRoute(..)
    , href
    , parseUrl
    , toHash
    )

import Domain exposing (ExerciseId, RoutineId)
import Id
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, int, s, top)


type Route
    = Home
    | Tags
    | Positions
    | Exercises
    | Exercise ExerciseId
    | ExerciseEditor (Maybe ExerciseId)
      -- Just exerciseId = focus on subset of routines containing this exercise
    | Routines (Maybe ExerciseId)
    | Routine RoutineId
    | RoutineEditor RoutineEditorRoute
    | Lessons
    | Images
    | Inspirations
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
        , P.map Inspirations (s "inspiration")

        -- Exercises
        , P.map Exercises (s "exercise")
        , P.map (Exercise << Id.fromInt) (s "exercise" </> int)
        , P.map (ExerciseEditor Nothing) (s "exercise" </> s "create")
        , P.map (ExerciseEditor << Just << Id.fromInt) (s "exercise" </> int </> s "edit")

        -- Routines
        , P.map (Routines << Just << Id.fromInt) (s "routine-by-exercise" </> int)
        , P.map (Routines Nothing) (s "routine")
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

        Inspirations ->
            "/inspiration"

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

        Routines maybeExerciseId ->
            case maybeExerciseId of
                Just exerciseId ->
                    "/routine-by-exercise/" ++ Id.toString exerciseId

                Nothing ->
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
