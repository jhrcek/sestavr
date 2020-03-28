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
        )
import Element as E exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Id exposing (IdDict)
import List.Extra as List


type alias Model =
    { routineExercises : List ExerciseId
    , dnd : DnDList.Model
    }


type alias Config msg =
    { msg : Msg -> msg
    }


init : Model
init =
    { routineExercises = []
    , dnd = dndSystem.model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd


type Msg
    = AddToRoutine ExerciseId
    | RemoveFromRoutine ExerciseId
    | DnD DnDList.Msg


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        AddToRoutine exerciseId ->
            ( { model | routineExercises = model.routineExercises ++ [ exerciseId ] }
            , Cmd.none
            )

        RemoveFromRoutine exerciseId ->
            ( { model
                | routineExercises =
                    List.filter (\eid -> eid /= exerciseId)
                        model.routineExercises
              }
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


editor : IdDict ExerciseIdTag Exercise -> Model -> Element Msg
editor exercises model =
    E.row
        [ Border.solid
        , Border.width 1
        ]
        [ E.column
            [ E.alignTop
            , E.width <| E.minimum 200 E.fill
            , E.height E.fill
            , Border.solid
            , Border.width 1
            ]
            (E.el [ Font.bold, E.padding 5 ] (E.text "Dostupné cviky")
                :: (Dict.Any.values exercises
                        |> List.filter (\e -> not <| List.member e.id model.routineExercises)
                        |> List.map
                            (\e ->
                                E.row [ E.alignRight ]
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
            [ E.alignTop
            , E.width <| E.minimum 200 E.fill
            , E.height E.fill
            , Border.solid
            , Border.width 1
            , E.inFront (ghostView model.dnd model.routineExercises exercises)
            ]
            (E.el [ Font.bold, E.padding 5 ] (E.text "Sestava")
                :: (model.routineExercises
                        |> List.filterMap (\eid -> Dict.Any.get eid exercises)
                        |> List.indexedMap (draggableExercise model.dnd)
                   )
            )
        ]


draggableExercise : DnDList.Model -> Int -> Exercise -> Element Msg
draggableExercise dndModel index exercise =
    let
        exId =
            Id.toString exercise.id
    in
    draggableExerciseElement exercise <|
        case dndSystem.info dndModel of
            Just { dragIndex } ->
                if dragIndex /= index then
                    List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dropEvents index exId

                else
                    [ Font.color (E.rgb255 156 156 156) ]

            Nothing ->
                List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dragEvents index exId


draggableExerciseElement : Exercise -> List (E.Attribute Msg) -> Element Msg
draggableExerciseElement exercise attrs =
    E.row attrs
        [ Input.button buttonAttrs
            { onPress = Just (RemoveFromRoutine exercise.id)
            , label = E.text "«"
            }
        , E.el [ E.padding 5 ] (E.text exercise.name)
        ]


ghostView : DnDList.Model -> List ExerciseId -> IdDict ExerciseIdTag Exercise -> Element Msg
ghostView dndModel routineExercises exercises =
    dndSystem.info dndModel
        |> Maybe.andThen (\{ dragIndex } -> List.getAt dragIndex routineExercises)
        |> Maybe.andThen (\exerciseId -> Dict.Any.get exerciseId exercises)
        |> Maybe.map
            (\exercise ->
                draggableExerciseElement exercise <|
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


dndConfig : DnDList.Config ExerciseId
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System ExerciseId Msg
dndSystem =
    DnDList.create dndConfig DnD
