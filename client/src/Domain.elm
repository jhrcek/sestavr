module Domain exposing
    ( Target
    , TargetId
    , TargetIdTag
    , targetDecoder
    )

import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)


type TargetIdTag
    = TargetIdTag


type alias TargetId =
    Id TargetIdTag


type alias Target =
    { id : TargetId
    , name : String
    }


targetDecoder : Decoder Target
targetDecoder =
    Decode.map2 Target
        (Decode.field "id" Id.decode)
        (Decode.field "name" Decode.string)
