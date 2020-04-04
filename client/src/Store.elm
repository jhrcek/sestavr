module Store exposing
    ( Msg
    , Store
    , createExercise
    , createPosition
    , createRoutine
    , createTarget
    , deleteExercise
    , deletePosition
    , deleteRoutine
    , deleteTarget
    , getExercises
    , getLessons
    , getPositions
    , getRoutines
    , getTargets
    , init
    , update
    , updateExercise
    , updatePosition
    , updateRoutine
    , updateTarget
    )

import Dict.Any
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , ExerciseIdTag
        , Lesson
        , LessonIdTag
        , Position
        , PositionId
        , PositionIdTag
        , Routine
        , RoutineId
        , RoutineIdTag
        , Target
        , TargetId
        , TargetIdTag
        )
import Http
import Http.Extra as Ht2 exposing (ApiCall)
import Id exposing (IdDict)
import Json.Decode as Decode


type Msg
    = -- Target
      TargetsFetched (ApiCall (List Target))
    | TargetCreated (ApiCall Target)
    | TargetDeleted (ApiCall TargetId)
    | TargetUpdated (ApiCall Target)
      -- Position
    | PositionsFetched (ApiCall (List Position))
    | PositionCreated (ApiCall Position)
    | PositionDeleted (ApiCall PositionId)
    | PositionUpdated (ApiCall Position)
      -- Exercise
    | ExercisesFetched (ApiCall (List Exercise))
    | ExerciseCreated (ApiCall Exercise)
    | ExerciseUpdated (ApiCall Exercise)
    | ExerciseDeleted (ApiCall ExerciseId)
      -- Routine
    | RoutinesFetched (ApiCall (List Routine))
    | RoutineCreated (ApiCall Routine)
    | RoutineUpdated (ApiCall Routine)
    | RoutineDeleted (ApiCall RoutineId)
      -- Lesson
    | LessonsFetched (ApiCall (List Lesson))


type alias Store =
    { targets : IdDict TargetIdTag Target
    , positions : IdDict PositionIdTag Position
    , exercises : IdDict ExerciseIdTag Exercise
    , routines : IdDict RoutineIdTag Routine
    , lessons : IdDict LessonIdTag Lesson
    }


init : Store
init =
    { targets = Id.emptyDict
    , positions = Id.emptyDict
    , exercises = Id.emptyDict
    , routines = Id.emptyDict
    , lessons = Id.emptyDict
    }


update : Msg -> Store -> ( Store, Maybe Ht2.Error )
update msg store =
    case msg of
        TargetsFetched result ->
            updateOrError result store (\targets s -> { s | targets = Id.buildDict targets })

        TargetCreated result ->
            updateOrError result store (\target s -> { s | targets = Dict.Any.insert target.id target store.targets })

        TargetDeleted result ->
            updateOrError result store (\targetId s -> { s | targets = Dict.Any.remove targetId store.targets })

        TargetUpdated result ->
            updateOrError result store (\target s -> { s | targets = Dict.Any.insert target.id target store.targets })

        PositionsFetched result ->
            updateOrError result store (\positions s -> { s | positions = Id.buildDict positions })

        PositionCreated result ->
            updateOrError result store (\position s -> { s | positions = Dict.Any.insert position.id position store.positions })

        PositionDeleted result ->
            updateOrError result store (\positionId s -> { s | positions = Dict.Any.remove positionId store.positions })

        PositionUpdated result ->
            updateOrError result store (\position s -> { s | positions = Dict.Any.insert position.id position store.positions })

        ExercisesFetched result ->
            updateOrError result store (\exercises s -> { s | exercises = Id.buildDict exercises })

        ExerciseCreated result ->
            updateOrError result store (\exercise s -> { s | exercises = Dict.Any.insert exercise.id exercise store.exercises })

        ExerciseUpdated result ->
            updateOrError result store (\exercise s -> { s | exercises = Dict.Any.insert exercise.id exercise store.exercises })

        ExerciseDeleted result ->
            updateOrError result store (\exerciseId s -> { s | exercises = Dict.Any.remove exerciseId store.exercises })

        RoutinesFetched result ->
            updateOrError result store (\routines s -> { s | routines = Id.buildDict routines })

        RoutineCreated result ->
            updateOrError result store (\routine s -> { s | routines = Dict.Any.insert routine.id routine store.routines })

        RoutineUpdated result ->
            updateOrError result store (\routine s -> { s | routines = Dict.Any.insert routine.id routine store.routines })

        RoutineDeleted result ->
            updateOrError result store (\routineId s -> { s | routines = Dict.Any.remove routineId store.routines })

        LessonsFetched result ->
            updateOrError result store (\lessons s -> { s | lessons = Id.buildDict lessons })


updateOrError : ApiCall a -> Store -> (a -> Store -> Store) -> ( Store, Maybe Ht2.Error )
updateOrError result store f =
    case result of
        Ok a ->
            ( f a store, Nothing )

        Err e ->
            ( store, Just e )



-- TARGET


getTargets : Cmd Msg
getTargets =
    Http.get
        { url = "/target"
        , expect =
            Ht2.expectJson
                TargetsFetched
                (Decode.list Domain.targetDecoder)
        }


createTarget : Target -> Cmd Msg
createTarget target =
    Http.post
        { url = "/target"
        , body = Http.jsonBody <| Domain.encodeTarget target
        , expect = Ht2.expectJson TargetCreated Domain.targetDecoder
        }


deleteTarget : TargetId -> Cmd Msg
deleteTarget targetId =
    Ht2.delete
        { baseUrl = "/target/"
        , resourceId = targetId
        , onResponse = TargetDeleted
        }


updateTarget : Target -> Cmd Msg
updateTarget target =
    Http.post
        { url = "/target/" ++ Id.toString target.id
        , body = Http.jsonBody <| Domain.encodeTarget target
        , expect = Ht2.expectWhatever (TargetUpdated << Result.map (\() -> target))
        }



-- POSITION


getPositions : Cmd Msg
getPositions =
    Http.get
        { url = "/position"
        , expect = Ht2.expectJson PositionsFetched (Decode.list Domain.positionDecoder)
        }


createPosition : Position -> Cmd Msg
createPosition position =
    Http.post
        { url = "/position"
        , body = Http.jsonBody <| Domain.encodePosition position
        , expect = Ht2.expectJson PositionCreated Domain.positionDecoder
        }


deletePosition : PositionId -> Cmd Msg
deletePosition positionId =
    Ht2.delete
        { baseUrl = "/position/"
        , resourceId = positionId
        , onResponse = PositionDeleted
        }


updatePosition : Position -> Cmd Msg
updatePosition position =
    Http.post
        { url = "/position/" ++ Id.toString position.id
        , body = Http.jsonBody <| Domain.encodePosition position
        , expect = Ht2.expectWhatever (PositionUpdated << Result.map (\() -> position))
        }



-- EXERCISE


getExercises : Cmd Msg
getExercises =
    Http.get
        { url = "/exercise"
        , expect = Ht2.expectJson ExercisesFetched (Decode.list Domain.exerciseDecoder)
        }


createExercise : Exercise -> Cmd Msg
createExercise exercise =
    Http.post
        { url = "/exercise"
        , body = Http.jsonBody <| Domain.encodeExercise exercise
        , expect = Ht2.expectJson ExerciseCreated Domain.exerciseDecoder
        }


updateExercise : Exercise -> Cmd Msg
updateExercise exercise =
    Http.post
        { url = "/exercise/" ++ Id.toString exercise.id
        , body = Http.jsonBody <| Domain.encodeExercise exercise
        , expect = Ht2.expectJson ExerciseUpdated Domain.exerciseDecoder
        }


deleteExercise : ExerciseId -> Cmd Msg
deleteExercise exerciseId =
    Ht2.delete
        { baseUrl = "/exercise/"
        , resourceId = exerciseId
        , onResponse = ExerciseDeleted
        }



-- ROUTINE


getRoutines : Cmd Msg
getRoutines =
    Http.get
        { url = "/routine"
        , expect = Ht2.expectJson RoutinesFetched (Decode.list Domain.routineDecoder)
        }


createRoutine : Routine -> Cmd Msg
createRoutine routine =
    Http.post
        { url = "/routine"
        , body = Http.jsonBody <| Domain.encodeRoutine routine
        , expect = Ht2.expectJson RoutineCreated Domain.routineDecoder
        }


updateRoutine : Routine -> Cmd Msg
updateRoutine routine =
    Http.post
        { url = "/routine/" ++ Id.toString routine.id
        , body = Http.jsonBody <| Domain.encodeRoutine routine
        , expect = Ht2.expectJson RoutineUpdated Domain.routineDecoder
        }


deleteRoutine : RoutineId -> Cmd Msg
deleteRoutine routineId =
    Ht2.delete
        { baseUrl = "/routine/"
        , resourceId = routineId
        , onResponse = RoutineDeleted
        }



-- LESSON


getLessons : Cmd Msg
getLessons =
    Http.get
        { url = "/lesson"
        , expect = Ht2.expectJson LessonsFetched (Decode.list Domain.lessonDecoder)
        }
