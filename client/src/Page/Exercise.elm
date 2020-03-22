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
        , TargetId
        , TargetIdTag
        )
import Element as E exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Id exposing (IdDict, IdSet)
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
    { createExercise : Exercise -> msg
    , updateExercise : Exercise -> msg

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


viewEditor : IdDict PositionIdTag Position -> Model -> Element Msg
viewEditor positions model =
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
        , positionRadios positions model.positionId

        --TODO target areas multiselect
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
    Input.radio []
        { onChange = PositionSelected
        , selected = maybeCurrentPosition
        , label = Input.labelLeft [ E.padding 3 ] (E.text "Pozice")
        , options =
            Dict.Any.values positions
                |> List.map (\p -> Input.option p.id (E.text p.name))
        }


viewList : IdDict ExerciseIdTag Exercise -> Element msg
viewList exercises =
    Dict.Any.values exercises
        |> List.map exerciseLink
        |> E.column []


exerciseLink : Exercise -> Element msg
exerciseLink exercise =
    E.row []
        [ E.link
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
        ]


lightBlue : E.Color
lightBlue =
    E.rgb255 18 147 216


view : IdDict PositionIdTag Position -> Exercise -> Element msg
view positions exercise =
    E.column [ E.width E.fill ]
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , E.el [ Font.size 28, Font.bold ] (E.text exercise.name)
        , E.el [ Font.size 20, Font.bold, Font.italic ]
            (E.text <| "Sanskrt : " ++ Maybe.withDefault "N/A" exercise.sanskritName)

        -- TODO improve how we deal with the cases when position not in store
        , E.el [] (E.text <| "Pozice: " ++ Maybe.withDefault "BUG!!!" (Maybe.map .name (Dict.Any.get exercise.positionId positions)))
        , E.paragraph []
            [ E.html <| Markdown.toHtml [] exercise.description ]
        , E.link
            [ E.padding 5
            , Border.solid
            , Border.width 1
            , Border.rounded 4
            , E.alignRight
            ]
            { url = Router.href (Router.ExerciseEditor exercise.id)
            , label = E.text "Upravit"
            }
        ]


backToList : Element msg
backToList =
    E.link
        [ E.mouseOver [ E.moveLeft 2 ], Font.color lightBlue ]
        { url = Router.href Exercises
        , label = E.text "« Zpět na seznam cviků"
        }
