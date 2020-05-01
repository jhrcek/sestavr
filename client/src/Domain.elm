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
    , Tag
    , TagId
    , TagIdTag
    , encodeExercise
    , encodeLesson
    , encodePosition
    , encodeRoutine
    , encodeTag
    , exerciseDecoder
    , lessonDecoder
    , positionDecoder
    , routineDecoder
    , tagDecoder
    )

import Id exposing (Id)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)



-- TAG


type TagIdTag
    = TagIdTag


type alias TagId =
    Id TagIdTag


type alias Tag =
    { id : TagId
    , name : String
    }


tagDecoder : Decoder Tag
tagDecoder =
    Decode.map2 Tag
        (Decode.field "id" Id.decode)
        (Decode.field "name" Decode.string)


encodeTag : Tag -> Value
encodeTag tag =
    Encode.object
        [ ( "name", Encode.string tag.name ) ]



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
    , image : Maybe String
    , description : String
    , positionId : PositionId
    , tagIds : List TagId
    }


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    Decode.map7 Exercise
        (Decode.field "exerciseId" Id.decode)
        (Decode.field "name" Decode.string)
        (Decode.field "sanskritName" <| Decode.nullable Decode.string)
        (Decode.field "image" <| Decode.nullable Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "positionId" Id.decode)
        (Decode.field "tagIds" <| Decode.list Id.decode)


encodeExercise : Exercise -> Value
encodeExercise exercise =
    Encode.object
        [ ( "exerciseId", Id.encode exercise.id )
        , ( "name", Encode.string exercise.name )
        , ( "sanskritName", Maybe.withDefault Encode.null <| Maybe.map Encode.string exercise.sanskritName )
        , ( "image", Maybe.withDefault Encode.null <| Maybe.map Encode.string exercise.image )
        , ( "description", Encode.string exercise.description )
        , ( "positionId", Id.encode exercise.positionId )
        , ( "tagIds", Encode.list Id.encode exercise.tagIds )
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


encodeLesson : Lesson -> Value
encodeLesson lesson =
    Encode.object
        [ ( "id", Id.encode lesson.id )
        , ( "routineId", Id.encode lesson.routineId )
        , ( "datetime", Iso8601.encode lesson.datetime )
        ]



-- The code below is to reduce the number of "unused" warnings


type Tags
    = ExerciseIdTag_ ExerciseIdTag
    | LessonIdTag_ LessonIdTag
    | PositionIdTag_ PositionIdTag
    | RoutineIdTag_ RoutineIdTag
    | TagIdTag_ TagIdTag


tags : List Tags
tags =
    [ ExerciseIdTag_ ExerciseIdTag
    , LessonIdTag_ LessonIdTag
    , PositionIdTag_ PositionIdTag
    , RoutineIdTag_ RoutineIdTag
    , TagIdTag_ TagIdTag
    ]
