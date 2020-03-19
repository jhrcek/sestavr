module Store exposing
    ( Msg
    , Store
    , createTarget
    , deleteTarget
    , getExercises
    , getTargets
    , init
    , update
    , updateTarget
    )

import Dict.Any
import Domain
    exposing
        ( Exercise
        , ExerciseIdTag
        , Target
        , TargetId
        , TargetIdTag
        )
import Http
import Http.Extra as Ht2
import Id exposing (IdDict)
import Json.Decode as Decode


type Msg
    = GotTargets (Result Ht2.Error (List Target))
    | TargetCreated (Result Ht2.Error Target)
    | TargetDeleted (Result Ht2.Error TargetId)
    | TargetUpdated (Result Ht2.Error Target)
    | GotExercises (Result Ht2.Error (List Exercise))


type alias Store =
    { targets : IdDict TargetIdTag Target
    , exercises : IdDict ExerciseIdTag Exercise
    }


init : Store
init =
    { targets = Id.emptyDict
    , exercises = Id.emptyDict
    }


update : Msg -> Store -> ( Store, Maybe Ht2.Error )
update msg store =
    case msg of
        GotTargets result ->
            updateOrError result store (\targets s -> { s | targets = Id.buildDict targets })

        TargetCreated result ->
            updateOrError result store (\target s -> { s | targets = Dict.Any.insert target.id target store.targets })

        TargetDeleted result ->
            updateOrError result store (\targetId s -> { s | targets = Dict.Any.remove targetId store.targets })

        TargetUpdated result ->
            updateOrError result store (\target s -> { s | targets = Dict.Any.insert target.id target store.targets })

        GotExercises result ->
            updateOrError result store (\exercises s -> { s | exercises = Id.buildDict exercises })


updateOrError : Result Ht2.Error a -> Store -> (a -> Store -> Store) -> ( Store, Maybe Ht2.Error )
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
                GotTargets
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
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/target/" ++ Id.toString targetId
        , expect = Ht2.expectWhatever (TargetDeleted << Result.map (\() -> targetId))
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


updateTarget : Target -> Cmd Msg
updateTarget target =
    Http.post
        { url = "/target/" ++ Id.toString target.id
        , body = Http.jsonBody <| Domain.encodeTarget target
        , expect = Ht2.expectWhatever (TargetUpdated << Result.map (\() -> target))
        }



-- EXERCISE


getExercises : Cmd Msg
getExercises =
    Http.get
        { url = "/exercise"
        , expect =
            Ht2.expectJson
                GotExercises
                (Decode.list Domain.exerciseDecoder)
        }
