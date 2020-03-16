module Store exposing
    ( Msg
    , Store
    , createTarget
    , deleteTarget
    , getTargets
    , init
    , update
    , updateTarget
    )

import Dict.Any
import Domain exposing (Target, TargetId, TargetIdTag)
import Http
import Id exposing (IdDict)
import Json.Decode as Decode


type Msg
    = GotTargets (Result Http.Error (List Target))
    | GotTarget (Result Http.Error Target)
    | TargetDeleted (Result Http.Error TargetId)
    | TargetUpdated (Result Http.Error Target)


type alias Store =
    { targets : IdDict TargetIdTag Target
    }


init : Store
init =
    { targets = Id.emptyDict
    }


update : Msg -> Store -> ( Store, Maybe Http.Error )
update msg store =
    case msg of
        GotTargets result ->
            updateOrError result store (\targets s -> { s | targets = Id.buildDict targets })

        GotTarget result ->
            updateOrError result store (\target s -> { s | targets = Dict.Any.insert target.id target store.targets })

        TargetDeleted result ->
            updateOrError result store (\targetId s -> { s | targets = Dict.Any.remove targetId store.targets })

        TargetUpdated result ->
            updateOrError result store (\target s -> { s | targets = Dict.Any.insert target.id target store.targets })


updateOrError : Result Http.Error a -> Store -> (a -> Store -> Store) -> ( Store, Maybe Http.Error )
updateOrError result store f =
    case result of
        Ok a ->
            ( f a store, Nothing )

        Err e ->
            ( store, Just e )


getTargets : Cmd Msg
getTargets =
    Http.get
        { url = "/target"
        , expect =
            Http.expectJson
                GotTargets
                (Decode.list Domain.targetDecoder)
        }


createTarget : Target -> Cmd Msg
createTarget target =
    Http.post
        { url = "/target"
        , body = Http.jsonBody <| Domain.encodeTarget target
        , expect = Http.expectJson GotTarget Domain.targetDecoder
        }


deleteTarget : TargetId -> Cmd Msg
deleteTarget targetId =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/target/" ++ Id.toString targetId
        , expect = Http.expectWhatever (TargetDeleted << Result.map (\() -> targetId))
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


updateTarget : Target -> Cmd Msg
updateTarget target =
    Http.post
        { url = "/target/" ++ Id.toString target.id
        , body = Http.jsonBody <| Domain.encodeTarget target
        , expect = Http.expectWhatever (TargetUpdated << Result.map (\() -> target))
        }
