module Domain exposing
    ( Exercise
    , ExerciseId
    , ExerciseIdTag
    , Lesson
    , LessonId
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
    , encodeExercise
    , encodePosition
    , encodeRoutine
    , encodeTarget
    , exerciseDecoder
    , lessonDecoder
    , positionDecoder
    , routineDecoder
    , targetDecoder
    )

import Id exposing (Id)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)



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



-- ROUTINE


type RoutineIdTag
    = RoutineIdTag


type alias RoutineId =
    Id RoutineIdTag


type alias Routine =
    { id : RoutineId
    , topic : String
    , exercises : List RoutineExercise
    }


type alias RoutineExercise =
    { exerciseId : ExerciseId
    , duration : Int
    }


routineDecoder : Decoder Routine
routineDecoder =
    Decode.map3 Routine
        (Decode.field "routineId" Id.decode)
        (Decode.field "topic" Decode.string)
        (Decode.field "exercises" (Decode.list routineExerciseDecoder))


routineExerciseDecoder : Decoder RoutineExercise
routineExerciseDecoder =
    Decode.map2 RoutineExercise
        (Decode.field "eirExerciseId" Id.decode)
        (Decode.field "eirDuration" Decode.int)


encodeRoutine : Routine -> Value
encodeRoutine routine =
    Encode.object
        [ ( "routineId", Id.encode routine.id )
        , ( "topic", Encode.string routine.topic )
        , ( "exercises", Encode.list encodeRoutineExercise routine.exercises )
        ]


encodeRoutineExercise : RoutineExercise -> Value
encodeRoutineExercise re =
    Encode.object
        [ ( "eirExerciseId", Id.encode re.exerciseId )
        , ( "eirDuration", Encode.int re.duration )
        ]



-- LESSON


type LessonIdTag
    = LessonIdTag


type alias LessonId =
    Id LessonIdTag


type alias Lesson =
    { id : LessonId
    , routineId : RoutineId
    , datetime : Posix
    }


lessonDecoder : Decoder Lesson
lessonDecoder =
    Decode.map3 Lesson
        (Decode.field "id" Id.decode)
        (Decode.field "routineId" Id.decode)
        (Decode.field "datetime" Iso8601.decoder)



-- The code below is to reduce the number of "unused" warnings


type Tags
    = ExerciseIdTag_ ExerciseIdTag
    | LessonIdTag_ LessonIdTag
    | PositionIdTag_ PositionIdTag
    | RoutineIdTag_ RoutineIdTag
    | TargetIdTag_ TargetIdTag


tags : List Tags
tags =
    [ ExerciseIdTag_ ExerciseIdTag
    , LessonIdTag_ LessonIdTag
    , PositionIdTag_ PositionIdTag
    , RoutineIdTag_ RoutineIdTag
    , TargetIdTag_ TargetIdTag
    ]
