module Page.Exercise exposing
    ( Config
    , Model
    , Msg
    , ValidationError(..)
    , emptyEditor
    , initEditor
    , listView
    , targetCheckboxes
    , update
    , validationErrorToString
    , view
    , viewEditor
    )

import Color
import Command
import Common
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
    { createExercise : Exercise -> msg
    , updateExercise : Exercise -> msg
    , deleteExercise : ExerciseId -> msg
    , validationError : ValidationError -> msg
    }


type ValidationError
    = PositionNotSet
    | NameEmpty


validationErrorToString : ValidationError -> String
validationErrorToString ve =
    case ve of
        PositionNotSet ->
            "Musíš zvolit pozici"

        NameEmpty ->
            "Musíš nastavit jméno"


updateOrCreate : Config msg -> Model -> Result ValidationError msg
updateOrCreate config model =
    (if String.isEmpty model.name then
        Err NameEmpty

     else
        Ok model.name
    )
        |> Result.andThen
            (\validName ->
                case model.positionId of
                    Nothing ->
                        Err PositionNotSet

                    Just positionId ->
                        let
                            ( request, id ) =
                                case model.exerciseId of
                                    Nothing ->
                                        ( config.createExercise, Id.fromInt -1 )

                                    Just exerciseId ->
                                        ( config.updateExercise, exerciseId )
                        in
                        Ok <|
                            request
                                { id = id
                                , name = validName
                                , sanskritName =
                                    if String.isEmpty model.sanskritName then
                                        Nothing

                                    else
                                        Just model.sanskritName
                                , description = model.description
                                , positionId = positionId
                                , targetIds = Set.Any.toList model.targetAreas
                                }
            )


initEditor : Exercise -> Model
initEditor exercise =
    { exerciseId = Just exercise.id
    , name = exercise.name
    , sanskritName = Maybe.withDefault "" exercise.sanskritName
    , description = exercise.description
    , positionId = Just exercise.positionId
    , targetAreas = Id.buildSet exercise.targetIds
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
            ( { model | name = newName }
            , Cmd.none
            )

        SetSanskritName newSanskritName ->
            ( { model | sanskritName = newSanskritName }
            , Cmd.none
            )

        SetDescription newDescription ->
            ( { model | description = newDescription }
            , Cmd.none
            )

        ToggleTargetId targetId ->
            ( { model | targetAreas = Set.Any.toggle targetId model.targetAreas }
            , Cmd.none
            )

        PositionSelected positionId ->
            ( { model | positionId = Just positionId }
            , Cmd.none
            )

        SaveExercise ->
            let
                command =
                    case updateOrCreate config model of
                        Err validationError ->
                            config.validationError validationError

                        Ok saveOrUpdateCmd ->
                            saveOrUpdateCmd
            in
            ( model
            , Command.perform command
            )


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
            , targetCheckboxes ToggleTargetId targets model.targetAreas
            ]
        , E.row [ E.alignRight, E.spacing 5 ]
            [ E.link Common.buttonAttrs
                { url =
                    case model.exerciseId of
                        Just exerciseId ->
                            Router.href (Router.Exercise exerciseId)

                        Nothing ->
                            Router.href Router.Exercises
                , label = E.text "Zrušit"
                }
            , Input.button Common.buttonAttrs
                { onPress = Just SaveExercise
                , label = E.text "Uložit"
                }
            ]
        ]


positionRadios : IdDict PositionIdTag Position -> Maybe PositionId -> Element Msg
positionRadios positions maybeCurrentPosition =
    E.el
        [ {- Workarkound - putting alignTop in radio's attributes doesn't align it -} E.alignTop ]
        (Input.radio []
            { onChange = PositionSelected
            , selected = maybeCurrentPosition
            , label =
                Input.labelAbove [ E.padding 3 ] <|
                    E.el [ Font.bold ] (E.text "Pozice")
            , options =
                List.map (\p -> Input.option p.id (E.text p.name)) <|
                    Dict.Any.values positions
            }
        )


targetCheckboxes : (TargetId -> msg) -> IdDict TargetIdTag Target -> IdSet TargetIdTag -> Element msg
targetCheckboxes onTargetToggle targets selectedTargets =
    let
        targetCheckbox target =
            Input.checkbox []
                { onChange = \_ -> onTargetToggle target.id
                , icon = Input.defaultCheckbox
                , checked = Set.Any.member target.id selectedTargets
                , label = Input.labelRight [] (E.text target.name)
                }
    in
    E.column []
        [ E.el [ E.padding 3, E.alignLeft, E.alignTop ] <|
            E.el [ Font.bold ] (E.text "Cílové oblasti")
        , Dict.Any.values targets
            |> List.greedyGroupsOf 10
            |> List.map (\group -> E.column [ E.alignTop ] (List.map targetCheckbox group))
            |> E.row [ E.width E.fill, E.alignTop ]
        ]


listView : IdDict ExerciseIdTag Exercise -> Element msg
listView exercises =
    Dict.Any.values exercises
        |> List.sortBy .name
        |> List.map exerciseLink
        |> (\exerciseLinks -> exerciseLinks ++ [ createExercisebutton ])
        |> (::) (Common.heading1 "Cviky")
        |> E.column []


exerciseLink : Exercise -> Element msg
exerciseLink exercise =
    E.link Common.linkAttrs
        { url = Router.href (Router.Exercise exercise.id)
        , label =
            E.text <|
                exercise.name
                    ++ Maybe.withDefault ""
                        (Maybe.map (\s -> " (" ++ s ++ ")")
                            exercise.sanskritName
                        )
        }


createExercisebutton : Element msg
createExercisebutton =
    E.link Common.buttonAttrs
        { url = Router.href (Router.ExerciseEditor Nothing)
        , label = E.text "Nový cvik"
        }


view :
    Config msg
    -> IdDict PositionIdTag Position
    -> IdDict TargetIdTag Target
    -> Exercise
    -> Element msg
view config positions targets exercise =
    E.column [ E.width E.fill ]
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , Common.heading1 exercise.name
        , E.el [ Font.size 20, Font.bold, Font.italic ]
            (E.text <| "Sanskrt : " ++ Maybe.withDefault "N/A" exercise.sanskritName)
        , E.row []
            [ case Dict.Any.get exercise.positionId positions of
                Just position ->
                    E.text <| "Pozice: " ++ position.name

                Nothing ->
                    E.none
            ]
        , E.row []
            [ E.text "Cílové oblasti: "
            , E.row [ E.spacing 5 ] <|
                List.map viewTargetArea <|
                    List.filterMap (\targetId -> Dict.Any.get targetId targets) exercise.targetIds
            ]
        , E.paragraph []
            [ E.html <| Markdown.toHtml [] exercise.description ]
        , E.row [ E.alignRight, E.spacing 5 ]
            [ E.link Common.buttonAttrs
                { url = Router.href <| Router.ExerciseEditor <| Just exercise.id
                , label = E.text "Upravit"
                }
            , Input.button Common.buttonAttrs
                { onPress = Just (config.deleteExercise exercise.id)
                , label = E.text "Odstranit"
                }
            ]
        ]


viewTargetArea : Target -> Element msg
viewTargetArea target =
    E.el
        [ Border.rounded 10
        , Border.solid
        , Border.color Color.black
        , Background.color (E.rgb255 255 192 203)
        , E.padding 3
        ]
        (E.text target.name)


backToList : Element msg
backToList =
    E.link Common.linkAttrs
        { url = Router.href Exercises
        , label = E.text "« Zpět na seznam cviků"
        }
