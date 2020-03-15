module Domain exposing
    ( Target
    , TargetId
    , TargetIdTag
    , encodeTarget
    , targetDecoder
    )

import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


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


encodeTarget : Target -> Value
encodeTarget target =
    Encode.object
        [ ( "name", Encode.string target.name ) ]
