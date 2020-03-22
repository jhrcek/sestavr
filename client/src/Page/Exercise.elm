module Page.Exercise exposing
    ( Config
    , Model
    , Msg
    , emptyEditor
    , initEditor
    , update
    , view
    , viewEditor
    , viewList
    )

import Command
import Dict.Any
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
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Id exposing (IdDict, IdSet)
import List.Extra as List
import Markdown
import Router exposing (Route(..))
import Set.Any


type alias Model =
    { exerciseId : Maybe ExerciseId
    , name : String
    , sanskritName : String
    , description : String
    , positionId : Maybe PositionId
    , targetAreas : IdSet TargetIdTag
    }


type Msg
    = SetName String
    | SetSanskritName String
    | SetDescription String
    | ToggleTargetId TargetId
    | PositionSelected PositionId
    | SaveExercise


type alias Config msg =
    { createExercise : Exercise -> List TargetId -> msg
    , updateExercise : Exercise -> List TargetId -> msg

    --, deleteExercise : ExerciseId -> msg
    }


type ValidationError
    = PositionNotSet


updateOrCreate : Config msg -> Model -> Result ValidationError msg
updateOrCreate config model =
    case model.positionId of
        Nothing ->
            Err PositionNotSet

        Just positionId ->
            Ok <|
                case model.exerciseId of
                    Nothing ->
                        config.createExercise
                            { id = Id.fromInt -1
                            , name = model.name
                            , sanskritName =
                                if String.isEmpty model.sanskritName then
                                    Nothing

                                else
                                    Just model.sanskritName
                            , description = model.description
                            , positionId = positionId
                            }
                            (Set.Any.toList model.targetAreas)

                    Just exerciseId ->
                        config.updateExercise
                            { id = exerciseId
                            , name = model.name
                            , sanskritName =
                                if String.isEmpty model.sanskritName then
                                    Nothing

                                else
                                    Just model.sanskritName
                            , description = model.description
                            , positionId = positionId
                            }
                            (Set.Any.toList model.targetAreas)


initEditor : Exercise -> Model
initEditor exercise =
    { exerciseId = Just exercise.id
    , name = exercise.name
    , sanskritName = Maybe.withDefault "" exercise.sanskritName
    , description = exercise.description
    , positionId = Just exercise.positionId
    , targetAreas = Id.emptySet
    }


emptyEditor : Model
emptyEditor =
    { exerciseId = Nothing
    , name = ""
    , sanskritName = ""
    , description = ""
    , positionId = Nothing
    , targetAreas = Id.emptySet
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        SetName newName ->
            ( { model | name = newName }, Cmd.none )

        SetSanskritName newSanskritName ->
            ( { model | sanskritName = newSanskritName }, Cmd.none )

        SetDescription newDescription ->
            ( { model | description = newDescription }, Cmd.none )

        ToggleTargetId targetId ->
            ( { model | targetAreas = Set.Any.toggle targetId model.targetAreas }, Cmd.none )

        PositionSelected positionId ->
            ( { model | positionId = Just positionId }
            , Cmd.none
            )

        SaveExercise ->
            let
                command =
                    case updateOrCreate config model of
                        Err validationError ->
                            -- TODO open modal
                            Cmd.none

                        Ok saveOrUpdateCmd ->
                            Command.perform saveOrUpdateCmd
            in
            ( model, command )


viewEditor :
    IdDict PositionIdTag Position
    -> IdDict TargetIdTag Target
    -> Model
    -> Element Msg
viewEditor positions targets model =
    let
        fieldWidth =
            E.width E.fill

        placeholder txt =
            Just <| Input.placeholder [] (E.text txt)

        buttonAttrs =
            [ E.padding 5
            , Border.solid
            , Border.width 1
            , Border.rounded 4
            ]
    in
    E.column [ E.width E.fill ]
        [ Input.text [ fieldWidth ]
            { onChange = SetName
            , text = model.name
            , placeholder = placeholder "Název"
            , label = Input.labelHidden "Název"
            }
        , Input.text [ fieldWidth ]
            { onChange = SetSanskritName
            , text = model.sanskritName
            , placeholder = placeholder "Sanskrt název"
            , label = Input.labelHidden "Sanskrt název"
            }
        , Input.multiline
            [ fieldWidth
            , E.height
                (E.fill
                    |> E.minimum 200
                    |> E.maximum 400
                )
            ]
            { onChange = SetDescription
            , text = model.description
            , placeholder = placeholder "Popis (markdown)"
            , label = Input.labelHidden "Popis"
            , spellcheck = False
            }
        , E.row [ E.spacing 30 ]
            [ positionRadios positions model.positionId
            , targetCheckboxes targets model.targetAreas
            ]
        , E.row [ E.alignRight, E.spacing 5 ]
            [ E.link buttonAttrs
                { url =
                    case model.exerciseId of
                        Just exerciseId ->
                            Router.href (Router.Exercise exerciseId)

                        Nothing ->
                            Router.href Router.Exercises
                , label = E.text "Zrušit"
                }
            , Input.button buttonAttrs
                { onPress = Just SaveExercise
                , label = E.text "Uložit"
                }
            ]
        ]


positionRadios : IdDict PositionIdTag Position -> Maybe PositionId -> Element Msg
positionRadios positions maybeCurrentPosition =
    Input.radio [ E.alignTop ]
        { onChange = PositionSelected
        , selected = maybeCurrentPosition
        , label = Input.labelLeft [ E.padding 3 ] (E.text "Pozice")
        , options =
            Dict.Any.values positions
                |> List.map (\p -> Input.option p.id (E.text p.name))
        }


targetCheckboxes : IdDict TargetIdTag Target -> IdSet TargetIdTag -> Element Msg
targetCheckboxes targets selectedTargets =
    let
        targetCheckbox target =
            Input.checkbox []
                { onChange = \_ -> ToggleTargetId target.id
                , icon = Input.defaultCheckbox
                , checked = Set.Any.member target.id selectedTargets
                , label = Input.labelRight [] (E.text target.name)
                }
    in
    E.row [ E.padding 8 ]
        [ E.el [ E.padding 3, E.alignTop ] (E.text "Cílové oblasti")
        , Dict.Any.values targets
            |> List.greedyGroupsOf 10
            |> List.map (\group -> E.column [ E.alignTop ] (List.map targetCheckbox group))
            |> E.row [ E.width E.fill, E.alignTop ]
        ]


viewList : IdDict ExerciseIdTag Exercise -> Element msg
viewList exercises =
    Dict.Any.values exercises
        |> List.map exerciseLink
        |> (\exerciseLinks -> exerciseLinks ++ [ createExercisebutton ])
        |> E.column []


exerciseLink : Exercise -> Element msg
exerciseLink exercise =
    E.link
        [ E.mouseOver [ E.moveRight 2 ]
        , Font.color lightBlue
        ]
        { url = Router.href (Router.Exercise exercise.id)
        , label =
            E.text
                (exercise.name
                    ++ Maybe.withDefault ""
                        (Maybe.map (\s -> " (" ++ s ++ ")")
                            exercise.sanskritName
                        )
                )
        }


createExercisebutton : Element msg
createExercisebutton =
    E.link [ Border.solid, Border.width 1, Border.rounded 4, E.padding 5 ]
        { url = Router.href (Router.ExerciseEditor Nothing)
        , label = E.text "Nový cvik"
        }


lightBlue : E.Color
lightBlue =
    E.rgb255 18 147 216


view :
    IdDict PositionIdTag Position
    -> IdDict TargetIdTag Target
    -> Exercise
    -> Element msg
view positions targets exercise =
    E.column [ E.width E.fill ]
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , E.el [ Font.size 28, Font.bold ] (E.text exercise.name)
        , E.el [ Font.size 20, Font.bold, Font.italic ]
            (E.text <| "Sanskrt : " ++ Maybe.withDefault "N/A" exercise.sanskritName)
        , E.row []
            [ E.text <| "Pozice: "

            -- TODO improve how we deal with the cases when position not in store
            , E.text <| Maybe.withDefault "BUG!!!" <| Maybe.map .name <| Dict.Any.get exercise.positionId positions
            ]
        , E.row []
            [ E.text "Cílové oblasti: "
            , E.row [] <|
                List.map targetAreaBadge
                    [{- TODO display selected target areas here -}]
            ]
        , E.paragraph []
            [ E.html <| Markdown.toHtml [] exercise.description ]
        , E.link
            [ E.padding 5
            , Border.solid
            , Border.width 1
            , Border.rounded 4
            , E.alignRight
            ]
            { url = Router.href <| Router.ExerciseEditor <| Just exercise.id
            , label = E.text "Upravit"
            }
        ]


targetAreaBadge : Target -> Element msg
targetAreaBadge target =
    E.el
        [ Border.rounded 8
        , Border.solid
        , Background.color (E.rgb255 192 109 152)
        ]
        (E.text target.name)


backToList : Element msg
backToList =
    E.link
        [ E.mouseOver [ E.moveLeft 2 ], Font.color lightBlue ]
        { url = Router.href Exercises
        , label = E.text "« Zpět na seznam cviků"
        }
