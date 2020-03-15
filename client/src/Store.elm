module Store exposing
    ( Msg
    , Store
    , createTarget
    , getTargets
    , init
    , update
    )

import Dict.Any
import Domain exposing (Target, TargetIdTag)
import Http
import Id exposing (IdDict)
import Json.Decode as Decode


type Msg
    = GotTargets (Result Http.Error (List Target))
    | GotTarget (Result Http.Error Target)


type alias Store =
    { targets : IdDict TargetIdTag Target
    }


init : Store
init =
    { targets = Id.emptyDict
    }


update : Msg -> Store -> Store
update msg store =
    case msg of
        GotTargets result ->
            case result of
                Ok targets ->
                    { store | targets = Id.buildDict targets }

                Err e ->
                    --TODO do something about the error
                    store

        GotTarget result ->
            case result of
                Ok target ->
                    { store | targets = Dict.Any.insert target.id target store.targets }

                Err e ->
                    --TODO do something about the error
                    store


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
