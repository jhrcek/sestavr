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
        , Position
        , PositionId
        , PositionIdTag
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
    , positionFilter : IdSet PositionIdTag
    }


type alias ExerciseInRoutine =
    { draggableItemId : DraggableItemId
    , exercise : Exercise
    , duration : Duration
    }


type alias DraggableItemId =
    Int


type alias Config msg =
    { msg : Msg -> msg
    }


init : Model
init =
    { routineExercises = []
    , dnd = dndSystem.model
    , targetFilter = Id.emptySet
    , positionFilter = Id.emptySet
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd


type Msg
    = AddToRoutine ExerciseId
    | RemoveFromRoutine ExerciseInRoutine
    | ToggleTargetId TargetId
    | TogglePositionId PositionId
    | ChangeDuration DraggableItemId String
    | ClearTargets
    | ClearPositions
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

        TogglePositionId positionId ->
            ( { model | positionFilter = Set.Any.toggle positionId model.positionFilter }
            , Cmd.none
            )

        ChangeDuration draggableItemId durationString ->
            ( { model
                | routineExercises =
                    case parseDuration durationString of
                        Nothing ->
                            model.routineExercises

                        Just newDuration ->
                            List.updateIf
                                (\exerciseInRoutine -> exerciseInRoutine.draggableItemId == draggableItemId)
                                (\exerciseIntRoutine -> { exerciseIntRoutine | duration = newDuration })
                                model.routineExercises
              }
            , Cmd.none
            )

        ClearTargets ->
            ( { model | targetFilter = Id.emptySet }
            , Cmd.none
            )

        ClearPositions ->
            ( { model | positionFilter = Id.emptySet }
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


{-| This is to allow numbers only input the value of which can be deleted (corresponding to 0)
-}
type Duration
    = Empty
    | Duration Int


parseDuration : String -> Maybe Duration
parseDuration str =
    if String.isEmpty str then
        Just Empty

    else
        Maybe.map Duration <| String.toInt str


durationToInt : Duration -> Int
durationToInt duration =
    case duration of
        Empty ->
            0

        Duration x ->
            x


addExercise : Exercise -> List ExerciseInRoutine -> List ExerciseInRoutine
addExercise exercise list =
    List.indexedMap
        (\idx eir -> { eir | draggableItemId = idx })
        (list ++ [ { draggableItemId = 0, exercise = exercise, duration = Duration 3 } ])


routineDurationMinutes : List ExerciseInRoutine -> Int
routineDurationMinutes =
    List.map (.duration >> durationToInt) >> List.sum


editor :
    IdDict ExerciseIdTag Exercise
    -> IdDict TargetIdTag Target
    -> IdDict PositionIdTag Position
    -> Model
    -> Element Msg
editor exercises targets positions model =
    let
        colAttrs maxWidth =
            [ E.alignTop
            , E.width <| E.maximum maxWidth E.fill
            , E.height E.fill
            , Border.solid
            , Border.width 1
            ]

        exerciseColumnWidth =
            460

        filteredExercises =
            Dict.Any.values exercises
                |> List.sortBy .name
                |> (if Set.Any.isEmpty model.targetFilter then
                        identity

                    else
                        -- Keep only exercises that target at least one area selected in targetFilter
                        List.filter (\exercise -> setAny (\targetId -> List.member targetId exercise.targetIds) model.targetFilter)
                   )
                |> (if Set.Any.isEmpty model.positionFilter then
                        identity

                    else
                        List.filter (\exercise -> Set.Any.member exercise.positionId model.positionFilter)
                   )
    in
    E.row
        [ Border.solid
        , Border.width 1
        , E.width <| E.maximum (200 + 2 * exerciseColumnWidth) E.fill
        ]
        [ E.column (E.paddingXY 5 0 :: colAttrs 200)
            [ Exercise.targetCheckboxes ToggleTargetId targets model.targetFilter
            , if Set.Any.isEmpty model.targetFilter then
                E.none

              else
                Input.button buttonAttrs
                    { onPress = Just ClearTargets, label = E.text "Zrušit výběr" }
            , positionCheckboxes positions model.positionFilter
            , if Set.Any.isEmpty model.positionFilter then
                E.none

              else
                Input.button buttonAttrs
                    { onPress = Just ClearPositions, label = E.text "Zrušit výběr" }
            , let
                filteredCount =
                    List.length filteredExercises

                totalCount =
                    Dict.Any.size exercises
              in
              if totalCount > List.length filteredExercises then
                E.paragraph []
                    [ E.text <|
                        String.fromInt filteredCount
                            ++ " / "
                            ++ String.fromInt totalCount
                            ++ " cviků odpovídá kriteriím"
                    ]

              else
                E.none
            ]
        , E.column (colAttrs exerciseColumnWidth)
            (E.el [ Font.bold, E.padding 5 ]
                (E.text "Dostupné cviky")
                :: List.map
                    (\exercise ->
                        E.row [ E.paddingXY 5 0, E.width E.fill ]
                            [ E.el [ E.padding 5 ] (E.text exercise.name)
                            , Input.button (E.alignRight :: buttonAttrs)
                                { onPress = Just (AddToRoutine exercise.id)
                                , label = E.text "»"
                                }
                            ]
                    )
                    filteredExercises
            )
        , E.column
            (E.inFront (ghostView model.dnd model.routineExercises) :: colAttrs exerciseColumnWidth)
            (E.el [ Font.bold, E.padding 5 ] (E.text "Sestava")
                :: List.indexedMap (draggableExercise model.dnd) model.routineExercises
                ++ [ E.el [ E.padding 5 ] <|
                        E.text <|
                            "Celková délka "
                                ++ String.fromInt (routineDurationMinutes model.routineExercises)
                                ++ " min"
                   ]
            )
        ]


positionCheckboxes : IdDict PositionIdTag Position -> IdSet PositionIdTag -> Element Msg
positionCheckboxes targets selectedPositions =
    let
        positionCheckbox position =
            Input.checkbox []
                { onChange = \_ -> TogglePositionId position.id
                , icon = Input.defaultCheckbox
                , checked = Set.Any.member position.id selectedPositions
                , label = Input.labelRight [] (E.text position.name)
                }
    in
    E.column []
        [ E.el [ E.padding 3, E.alignLeft, E.alignTop ] <|
            E.el [ Font.bold ] (E.text "Typ pozice")
        , Dict.Any.values targets
            |> List.map positionCheckbox
            |> E.column [ E.alignTop ]
        ]


setAny : (a -> Bool) -> Set.Any.AnySet Int a -> Bool
setAny p set =
    Set.Any.foldl (\a acc -> p a || acc) False set


draggableExercise : DnDList.Model -> Int -> ExerciseInRoutine -> Element Msg
draggableExercise dndModel index exerciseInRoutine =
    let
        exId =
            String.fromInt exerciseInRoutine.draggableItemId
    in
    E.row [ E.width E.fill ]
        [ draggableExerciseElement exerciseInRoutine <|
            case dndSystem.info dndModel of
                Just { dragIndex } ->
                    if dragIndex /= index then
                        List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dropEvents index exId

                    else
                        [ Font.color (E.rgb255 156 156 156) ]

                Nothing ->
                    List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dragEvents index exId
        , Input.text
            [ E.alignRight
            , E.width (E.px 50)
            , E.height (E.px 30)
            , E.padding 4
            , E.htmlAttribute (Attr.type_ "number")
            ]
            { onChange = ChangeDuration exerciseInRoutine.draggableItemId
            , text =
                case exerciseInRoutine.duration of
                    Empty ->
                        ""

                    Duration minutes ->
                        String.fromInt minutes
            , placeholder = Nothing
            , label = Input.labelHidden "duration"
            }
        ]


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
