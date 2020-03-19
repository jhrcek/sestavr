module Domain exposing
    ( Exercise
    , ExerciseId
    , ExerciseIdTag
    , PositionId
    , Target
    , TargetId
    , TargetIdTag
    , encodeTarget
    , exerciseDecoder
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


type PositionIdTag
    = PositionIdTag


type alias PositionId =
    Id PositionIdTag


type ExerciseIdTag
    = ExerciseIdTag


type alias ExerciseId =
    Id ExerciseIdTag


type alias Exercise =
    { id : ExerciseId
    , name : String
    , sanskritName : Maybe String
    , description : String
    , positionId : PositionId
    }


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    Decode.map5 Exercise
        (Decode.field "id" Id.decode)
        (Decode.field "name" Decode.string)
        (Decode.field "sanskritName" <| Decode.nullable Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "positionId" Id.decode)



-- The code below is to reduce the number of "unused" warnings


type Tags
    = PositionIdTag_ PositionIdTag
    | ExerciseIdTag_ ExerciseIdTag
    | TargetIdTag_ TargetIdTag


tags : List Tags
tags =
    [ PositionIdTag_ PositionIdTag
    , ExerciseIdTag_ ExerciseIdTag
    , TargetIdTag_ TargetIdTag
    ]
