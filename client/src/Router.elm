module Router exposing
    ( Route(..)
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
    | Targets
    | Positions
    | Exercises
    | Exercise ExerciseId
    | ExerciseEditor (Maybe ExerciseId)
    | Routines
    | Routine RoutineId
    | RoutineEditor (Maybe RoutineId)
    | NotFound String


route : Parser (Route -> a) a
route =
    P.oneOf
        [ P.map Home top
        , P.map Targets (s "target")
        , P.map Positions (s "position")

        -- Exercises
        , P.map Exercises (s "exercise")
        , P.map (Exercise << Id.fromInt) (s "exercise" </> int)
        , P.map (ExerciseEditor Nothing) (s "exercise" </> s "create")
        , P.map (ExerciseEditor << Just << Id.fromInt) (s "exercise" </> int </> s "edit")

        -- Routines
        , P.map Routines (s "routine")
        , P.map (Routine << Id.fromInt) (s "routine" </> int)
        , P.map (RoutineEditor Nothing) (s "routine" </> s "create")
        , P.map (RoutineEditor << Just << Id.fromInt) (s "routine" </> int </> s "edit")
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

        Targets ->
            "/target"

        Positions ->
            "/position"

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

        RoutineEditor maybeRoutineId ->
            case maybeRoutineId of
                Nothing ->
                    "/routine/create"

                Just routineId ->
                    "/routine/" ++ Id.toString routineId ++ "/edit"

        NotFound bad ->
            bad


href : Route -> String
href r =
    "#" ++ toHash r
