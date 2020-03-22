module Store exposing
    ( Msg
    , Store
    , createPosition
    , createTarget
    , deletePosition
    , deleteTarget
    , getExercises
    , getPositions
    , getTargets
    , init
    , update
    , updateExercise
    , updatePosition
    , updateTarget
    )

import Dict.Any
import Domain exposing (Exercise, ExerciseIdTag, Position, PositionId, PositionIdTag, Target, TargetId, TargetIdTag)
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
    | ExerciseUpdate (ApiCall Exercise)


type alias Store =
    { targets : IdDict TargetIdTag Target
    , positions : IdDict PositionIdTag Position
    , exercises : IdDict ExerciseIdTag Exercise
    }


init : Store
init =
    { targets = Id.emptyDict
    , positions = Id.emptyDict
    , exercises = Id.emptyDict
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

        ExerciseUpdate result ->
            updateOrError result store (\exercise s -> { s | exercises = Dict.Any.insert exercise.id exercise store.exercises })


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


updateExercise : Exercise -> Cmd Msg
updateExercise exercise =
    Http.post
        { url = "/exercise/" ++ Id.toString exercise.id
        , body = Http.jsonBody <| Domain.encodeExercise exercise
        , expect = Ht2.expectWhatever (ExerciseUpdate << Result.map (\() -> exercise))
        }
