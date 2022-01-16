module Domain exposing
    ( Exercise
    , ExerciseId
    , ExerciseIdTag
    , ImageVerificationResult
    , Inspiration
    , InspirationId
    , InspirationIdTag
    , ItemInRoutineId(..)
    , Lesson
    , LessonId
    , LessonIdTag
    , Position
    , PositionId
    , PositionIdTag
    , Routine
    , RoutineId
    , RoutineIdTag
    , RoutineItem
    , Tag
    , TagId
    , TagIdTag
    , emptyImageVerificationResult
    , encodeExercise
    , encodeInspiration
    , encodeLesson
    , encodePosition
    , encodeRoutine
    , encodeTag
    , exerciseDecoder
    , imageVerificationResultDecoder
    , inspirationDecoder
    , lessonDecoder
    , positionDecoder
    , removeUnusedImage
    , routineDecoder
    , tagDecoder
    )

import Id exposing (Id, IdDict)
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
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



-- COMMENT


type CommentIdTag
    = CommentIdTag


type alias CommentId =
    Id CommentIdTag



-- TODO comment
-- ROUTINE


type RoutineIdTag
    = RoutineIdTag


type alias RoutineId =
    Id RoutineIdTag


type alias Routine =
    { id : RoutineId
    , topic : String

    -- TODO rename to items
    , exercises : List RoutineItem
    }


type ItemInRoutineId
    = RiExercise ExerciseId
    | RiComment CommentId


type alias RoutineItem =
    { itemId : ItemInRoutineId
    , duration : Int
    }


itemInRoutineIdDecoder : Decoder ItemInRoutineId
itemInRoutineIdDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "RiExercise" ->
                        Decode.field "contents" <| Decode.map RiExercise Id.decode

                    "RiComment" ->
                        Decode.field "contents" <| Decode.map RiComment Id.decode

                    bad ->
                        Decode.fail <| "Unexpected ItemInRoutineId tag: " ++ bad
            )


encodeItemInRoutineId : ItemInRoutineId -> Value
encodeItemInRoutineId i =
    case i of
        RiExercise eid ->
            Encode.object
                [ ( "tag", Encode.string "RiExercise" )
                , ( "contents", Id.encode eid )
                ]

        RiComment cid ->
            Encode.object
                [ ( "tag", Encode.string "RiComment" )
                , ( "contents", Id.encode cid )
                ]


routineDecoder : Decoder Routine
routineDecoder =
    Decode.map3 Routine
        (Decode.field "routineId" Id.decode)
        (Decode.field "topic" Decode.string)
        (Decode.field "rweExercises" (Decode.list routineItemDecoder))


routineItemDecoder : Decoder RoutineItem
routineItemDecoder =
    Decode.map2 RoutineItem
        (Decode.field "eirItemId" itemInRoutineIdDecoder)
        (Decode.field "eirDuration" Decode.int)


encodeRoutine : Routine -> Value
encodeRoutine routine =
    Encode.object
        [ ( "routineId", Id.encode routine.id )
        , ( "topic", Encode.string routine.topic )
        , ( "rweExercises", Encode.list encodeRoutineItem routine.exercises )
        ]


encodeRoutineItem : RoutineItem -> Value
encodeRoutineItem re =
    Encode.object
        [ ( "eirItemId", encodeItemInRoutineId re.itemId )
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



-- IMAGE


type alias ImageVerificationResult =
    { invalidLinks : IdDict ExerciseIdTag (List String)
    , unusedImages : List String
    , knownImages : List String
    }


emptyImageVerificationResult : ImageVerificationResult
emptyImageVerificationResult =
    ImageVerificationResult Id.emptyDict [] []


removeUnusedImage : String -> ImageVerificationResult -> ImageVerificationResult
removeUnusedImage imageFileName ivr =
    { ivr
        | unusedImages = List.remove imageFileName ivr.unusedImages
        , knownImages = List.remove imageFileName ivr.knownImages
    }


imageVerificationResultDecoder : Decoder ImageVerificationResult
imageVerificationResultDecoder =
    Decode.map3 ImageVerificationResult
        (Decode.field "invalidLinks" <|
            Decode.map Id.dictFromList <|
                Decode.list
                    (Decode.map2 Tuple.pair
                        (Decode.field "exerciseId" Id.decode)
                        (Decode.field "images" (Decode.list Decode.string))
                    )
        )
        (Decode.field "unusedImages" <| Decode.list Decode.string)
        (Decode.field "knownImages" <| Decode.list Decode.string)



-- INSPIRATION


type InspirationIdTag
    = InspirationIdTag


type alias InspirationId =
    Id InspirationIdTag


type alias Inspiration =
    { id : InspirationId
    , monthNumber : Int
    , description : String
    }


inspirationDecoder : Decoder Inspiration
inspirationDecoder =
    Decode.map3 Inspiration
        (Decode.field "id" Id.decode)
        (Decode.field "monthNumber" Decode.int)
        (Decode.field "description" Decode.string)


encodeInspiration : Inspiration -> Value
encodeInspiration inspiration =
    Encode.object
        [ ( "id", Id.encode inspiration.id )
        , ( "monthNumber", Encode.int inspiration.monthNumber )
        , ( "description", Encode.string inspiration.description )
        ]



-- The code below is to reduce the number of "unused" warnings


type Tags
    = ExerciseIdTag_ ExerciseIdTag
    | InspirationIdTag_ InspirationIdTag
    | LessonIdTag_ LessonIdTag
    | PositionIdTag_ PositionIdTag
    | RoutineIdTag_ RoutineIdTag
    | TagIdTag_ TagIdTag
    | CommentIdTag_ CommentIdTag


tags : List Tags
tags =
    [ ExerciseIdTag_ ExerciseIdTag
    , InspirationIdTag_ InspirationIdTag
    , LessonIdTag_ LessonIdTag
    , PositionIdTag_ PositionIdTag
    , RoutineIdTag_ RoutineIdTag
    , TagIdTag_ TagIdTag
    , CommentIdTag_ CommentIdTag
    ]
