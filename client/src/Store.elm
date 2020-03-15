module Store exposing
    ( Msg
    , Store
    , getTargets
    , init
    , update
    )

import Domain exposing (Target, TargetIdTag)
import Http
import Id exposing (IdDict)
import Json.Decode as Decode


type Msg
    = GotTargets (Result Http.Error (List Target))


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


getTargets : (Msg -> msg) -> Cmd msg
getTargets wrap =
    Http.get
        { url = "/target"
        , expect =
            Http.expectJson
                (wrap << GotTargets)
                (Decode.list Domain.targetDecoder)
        }
