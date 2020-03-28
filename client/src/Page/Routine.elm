module Page.Routine exposing
    ( Model
    , Msg
    , editor
    , init
    , update
    )

import Dict.Any
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , ExerciseIdTag
        , Position
        , PositionId
        , PositionIdTag
        )
import Element as E exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Id exposing (IdDict)


type alias Model =
    { routineExercises : List ExerciseId
    }


init : Model
init =
    { routineExercises = []
    }


type Msg
    = AddToRoutine ExerciseId
    | RemoveFromRoutine ExerciseId


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddToRoutine exerciseId ->
            { model | routineExercises = model.routineExercises ++ [ exerciseId ] }

        RemoveFromRoutine exerciseId ->
            { model
                | routineExercises =
                    List.filter (\eid -> eid /= exerciseId)
                        model.routineExercises
            }


editor : IdDict ExerciseIdTag Exercise -> Model -> Element Msg
editor exercises model =
    E.row
        [ Border.solid
        , Border.width 1
        ]
        [ E.column
            [ E.alignTop
            , E.width <| E.minimum 200 E.fill
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
            ]
            (E.el [ Font.bold, E.padding 5 ] (E.text "Sestava")
                :: (model.routineExercises
                        |> List.filterMap (\eid -> Dict.Any.get eid exercises)
                        |> List.map
                            (\e ->
                                E.row []
                                    [ Input.button buttonAttrs
                                        { onPress = Just (RemoveFromRoutine e.id)
                                        , label = E.text "«"
                                        }
                                    , E.el [ E.padding 5 ] (E.text e.name)
                                    ]
                            )
                   )
            )
        ]


buttonAttrs : List (E.Attribute msg)
buttonAttrs =
    [ E.padding 5
    , Border.solid
    , Border.width 1
    , Border.rounded 4
    ]
