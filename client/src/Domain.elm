module Domain exposing
    ( Exercise
    , ExerciseId
    , ExerciseIdTag
    , Position
    , PositionId
    , PositionIdTag
    , Target
    , TargetId
    , TargetIdTag
    , encodeExercise
    , encodePosition
    , encodeTarget
    , exerciseDecoder
    , positionDecoder
    , targetDecoder
    )

import Id exposing (Id)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TARGET


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



-- POSITION


type PositionIdTag
    = PositionIdTag


type alias PositionId =
    Id PositionIdTag


type alias Position =
    { id : PositionId
    , name : String
    }


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "id" Id.decode)
        (Decode.field "name" Decode.string)


encodePosition : Position -> Value
encodePosition position =
    Encode.object
        [ ( "name", Encode.string position.name ) ]



-- EXERCISE


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
    , targetIds : List TargetId
    }


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    Decode.map6 Exercise
        (Decode.field "exerciseId" Id.decode)
        (Decode.field "name" Decode.string)
        (Decode.field "sanskritName" <| Decode.nullable Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "positionId" Id.decode)
        (Decode.field "targetIds" <| Decode.list Id.decode)


encodeExercise : Exercise -> Value
encodeExercise exercise =
    Encode.object
        [ ( "exerciseId", Id.encode exercise.id )
        , ( "name", Encode.string exercise.name )
        , ( "sanskritName", Maybe.withDefault Encode.null <| Maybe.map Encode.string exercise.sanskritName )
        , ( "description", Encode.string exercise.description )
        , ( "positionId", Id.encode exercise.positionId )
        , ( "targetIds", Encode.list Id.encode exercise.targetIds )
        ]



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
