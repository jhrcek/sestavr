module Page.Routine exposing
    ( Config
    , Model
    , Msg
    , editor
    , init
    , subscriptions
    , update
    )

import Dict.Any
import DnDList
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , ExerciseIdTag
        , Target
        , TargetId
        , TargetIdTag
        )
import Element as E exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Id exposing (IdDict, IdSet)
import List.Extra as List
import Page.Exercise as Exercise
import Set.Any


type alias Model =
    { routineExercises : List ExerciseInRoutine
    , dnd : DnDList.Model
    , targetFilter : IdSet TargetIdTag
    }


type alias ExerciseInRoutine =
    { draggableItemId : String
    , exercise : Exercise
    , duration : Int
    }


type alias Config msg =
    { msg : Msg -> msg
    }


init : Model
init =
    { routineExercises = []
    , dnd = dndSystem.model
    , targetFilter = Id.emptySet
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd


type Msg
    = AddToRoutine ExerciseId
    | RemoveFromRoutine ExerciseInRoutine
    | ToggleTargetId TargetId
    | ClearTargets
    | DnD DnDList.Msg


update : Config msg -> IdDict ExerciseIdTag Exercise -> Msg -> Model -> ( Model, Cmd msg )
update config exercises msg model =
    case msg of
        AddToRoutine exerciseId ->
            ( { model
                | routineExercises =
                    (Dict.Any.get exerciseId exercises
                        |> Maybe.map addExercise
                        |> Maybe.withDefault identity
                    )
                        model.routineExercises
              }
            , Cmd.none
            )

        RemoveFromRoutine exerciseInRoutine ->
            ( { model
                | routineExercises =
                    List.filter (\eir -> eir /= exerciseInRoutine)
                        model.routineExercises
              }
            , Cmd.none
            )

        ToggleTargetId targetId ->
            ( { model | targetFilter = Set.Any.toggle targetId model.targetFilter }
            , Cmd.none
            )

        ClearTargets ->
            ( { model | targetFilter = Id.emptySet }
            , Cmd.none
            )

        DnD dndMsg ->
            let
                ( dnd, routineExercises ) =
                    dndSystem.update dndMsg model.dnd model.routineExercises
            in
            ( { model
                | dnd = dnd
                , routineExercises = routineExercises
              }
            , Cmd.map config.msg <| dndSystem.commands model.dnd
            )


addExercise : Exercise -> List ExerciseInRoutine -> List ExerciseInRoutine
addExercise exercise list =
    List.indexedMap
        (\idx eir -> { eir | draggableItemId = String.fromInt idx })
        (list ++ [ { draggableItemId = "", exercise = exercise, duration = 0 } ])


editor :
    IdDict ExerciseIdTag Exercise
    -> IdDict TargetIdTag Target
    -> Model
    -> Element Msg
editor exercises targets model =
    let
        colAttrs =
            [ E.alignTop
            , E.width <| E.minimum 200 E.fill
            , E.height E.fill
            , Border.solid
            , Border.width 1
            ]
    in
    E.row
        [ Border.solid
        , Border.width 1
        ]
        [ E.column colAttrs
            [ Exercise.targetCheckboxes ToggleTargetId targets model.targetFilter
            , if Set.Any.isEmpty model.targetFilter then
                E.none

              else
                Input.button buttonAttrs
                    { onPress = Just ClearTargets, label = E.text "Zrušit výběr" }
            ]
        , E.column colAttrs
            (E.el [ Font.bold, E.padding 5 ] (E.text "Dostupné cviky")
                :: (Dict.Any.values exercises
                        |> (if Set.Any.isEmpty model.targetFilter then
                                identity

                            else
                                -- Keep only exercises that target at least one area selected in targetFilter
                                List.filter (\exercise -> setAny (\targetId -> List.member targetId exercise.targetIds) model.targetFilter)
                           )
                        |> List.map
                            (\e ->
                                E.row [ E.alignRight, E.paddingXY 5 0 ]
                                    [ E.el [ E.padding 5 ] (E.text e.name)
                                    , Input.button buttonAttrs
                                        { onPress = Just (AddToRoutine e.id)
                                        , label = E.text "»"
                                        }
                                    ]
                            )
                   )
            )
        , E.column
            (E.inFront (ghostView model.dnd model.routineExercises) :: colAttrs)
            (E.el [ Font.bold, E.padding 5 ] (E.text "Sestava")
                :: List.indexedMap (draggableExercise model.dnd) model.routineExercises
            )
        ]


setAny : (a -> Bool) -> Set.Any.AnySet Int a -> Bool
setAny p set =
    Set.Any.foldl (\a acc -> p a || acc) False set


draggableExercise : DnDList.Model -> Int -> ExerciseInRoutine -> Element Msg
draggableExercise dndModel index exerciseInRoutine =
    let
        exId =
            exerciseInRoutine.draggableItemId
    in
    draggableExerciseElement exerciseInRoutine <|
        case dndSystem.info dndModel of
            Just { dragIndex } ->
                if dragIndex /= index then
                    List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dropEvents index exId

                else
                    [ Font.color (E.rgb255 156 156 156) ]

            Nothing ->
                List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dragEvents index exId


draggableExerciseElement : ExerciseInRoutine -> List (E.Attribute Msg) -> Element Msg
draggableExerciseElement eir attrs =
    E.row (E.paddingXY 5 0 :: attrs)
        [ Input.button buttonAttrs
            { onPress = Just (RemoveFromRoutine eir)
            , label = E.text "«"
            }
        , E.el [ E.padding 5 ] (E.text eir.exercise.name)
        ]


ghostView : DnDList.Model -> List ExerciseInRoutine -> Element Msg
ghostView dndModel routineExercises =
    dndSystem.info dndModel
        |> Maybe.andThen (\{ dragIndex } -> List.getAt dragIndex routineExercises)
        |> Maybe.map
            (\exerciseInRoutine ->
                draggableExerciseElement exerciseInRoutine <|
                    List.map E.htmlAttribute <|
                        dndSystem.ghostStyles dndModel
            )
        |> Maybe.withDefault E.none


buttonAttrs : List (E.Attribute msg)
buttonAttrs =
    [ E.padding 5
    , Border.solid
    , Border.width 1
    , Border.rounded 4
    ]


dndConfig : DnDList.Config ExerciseInRoutine
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System ExerciseInRoutine Msg
dndSystem =
    DnDList.create dndConfig DnD
