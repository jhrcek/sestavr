module Page.Exercise exposing
    ( Config
    , Model
    , Msg
    , ValidationError(..)
    , emptyEditor
    , initEditor
    , listView
    , tagCheckboxes
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
        , Tag
        , TagId
        , TagIdTag
        )
import Element as E exposing (Element)
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
    , tags : IdSet TagIdTag
    }


type Msg
    = SetName String
    | SetSanskritName String
    | SetDescription String
    | ToggleTagId TagId
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
                                , tagIds = Set.Any.toList model.tags
                                }
            )


initEditor : Exercise -> Model
initEditor exercise =
    { exerciseId = Just exercise.id
    , name = exercise.name
    , sanskritName = Maybe.withDefault "" exercise.sanskritName
    , description = exercise.description
    , positionId = Just exercise.positionId
    , tags = Id.buildSet exercise.tagIds
    }


emptyEditor : Model
emptyEditor =
    { exerciseId = Nothing
    , name = ""
    , sanskritName = ""
    , description = "![asana](asana)"
    , positionId = Nothing
    , tags = Id.emptySet
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

        ToggleTagId tagId ->
            ( { model | tags = Set.Any.toggle tagId model.tags }
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
    -> IdDict TagIdTag Tag
    -> Model
    -> Element Msg
viewEditor positions tags model =
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
            , tagCheckboxes ToggleTagId 10 tags model.tags
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
                    List.sortBy .name <|
                        Dict.Any.values positions
            }
        )


tagCheckboxes : (TagId -> msg) -> Int -> IdDict TagIdTag Tag -> IdSet TagIdTag -> Element msg
tagCheckboxes onTagToggle checkboxesPerColumn tags selectedTags =
    let
        tagCheckbox tag =
            Input.checkbox []
                { onChange = \_ -> onTagToggle tag.id
                , icon = Input.defaultCheckbox
                , checked = Set.Any.member tag.id selectedTags
                , label = Input.labelRight [] (E.text tag.name)
                }
    in
    E.column [ E.alignTop ]
        [ E.el [ E.padding 3, Font.bold ]
            (E.text "Tagy")
        , Dict.Any.values tags
            |> List.sortBy .name
            |> List.greedyGroupsOf checkboxesPerColumn
            |> List.map (\group -> E.column [ E.alignTop ] (List.map tagCheckbox group))
            |> E.row [ E.width E.fill, E.alignTop ]
        ]


listView : IdDict ExerciseIdTag Exercise -> Element msg
listView exercises =
    let
        cell =
            E.el
                [ Border.solid
                , Border.width 1
                , E.width E.fill
                , E.padding 3
                , Border.color Color.lightGrey
                ]
    in
    E.column []
        [ Common.heading1 "Cviky"
        , E.table
            [ Border.solid
            , Border.width 1
            , Border.color Color.lightGrey
            ]
            { data =
                Dict.Any.values exercises
                    |> List.sortBy .name
            , columns =
                [ { header = cell <| E.text "Název"
                  , width = E.fill
                  , view =
                        \exercise -> cell <| exerciseLink exercise
                  }
                , { header = cell <| E.text "Sanskrt"
                  , width = E.fill
                  , view =
                        \exercise ->
                            Maybe.map E.text exercise.sanskritName
                                |> Maybe.withDefault (E.text "-")
                                |> cell
                  }
                ]
            }
        , createExercisebutton
        ]


exerciseLink : Exercise -> Element msg
exerciseLink exercise =
    E.link
        (E.paddingEach { top = 0, right = 10, bottom = 0, left = 0 }
            :: Common.linkAttrs
        )
        { url = Router.href (Router.Exercise exercise.id)
        , label = E.text exercise.name
        }


createExercisebutton : Element msg
createExercisebutton =
    E.el [ E.paddingXY 0 5 ]
        (E.link Common.buttonAttrs
            { url = Router.href (Router.ExerciseEditor Nothing)
            , label = E.text "Vytvořit cvik"
            }
        )


view :
    Config msg
    -> IdDict PositionIdTag Position
    -> IdDict TagIdTag Tag
    -> Exercise
    -> Element msg
view config positions tags exercise =
    E.column [ E.width E.fill, E.spacing 10 ]
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , Common.heading1 exercise.name
        , E.el [ Font.size 20, Font.bold, Font.italic ]
            (E.text <| "Sanskrt: " ++ Maybe.withDefault "N/A" exercise.sanskritName)
        , E.row []
            [ case Dict.Any.get exercise.positionId positions of
                Just position ->
                    E.text <| "Pozice: " ++ position.name

                Nothing ->
                    E.none
            ]
        , E.row []
            [ E.text <|
                "Tagy: "
                    ++ String.join ", "
                        (List.sort <|
                            List.map .name <|
                                List.filterMap (\tagId -> Dict.Any.get tagId tags) exercise.tagIds
                        )
            ]
        , E.row [ E.spacing 5 ]
            [ E.link Common.buttonAttrs
                { url = Router.href <| Router.ExerciseEditor <| Just exercise.id
                , label = E.text "Upravit"
                }
            , Input.button Common.buttonAttrs
                { onPress = Just (config.deleteExercise exercise.id)
                , label = E.text "Odstranit"
                }
            ]
        , E.paragraph []
            [ E.html <| Markdown.toHtml [] exercise.description ]
        ]


backToList : Element msg
backToList =
    E.link Common.linkAttrs
        { url = Router.href Exercises
        , label = E.text "« Zpět na seznam cviků"
        }
