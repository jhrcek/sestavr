module Page.Exercise exposing
    ( Config
    , Model
    , Msg
    , ValidationError(..)
    , emptyEditor
    , imagePreview
    , initEditor
    , listWithDetail
    , previewImageSize
    , tagCheckboxes
    , update
    , validationErrorToString
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
        , ImageVerificationResult
        , Position
        , PositionId
        , PositionIdTag
        , Tag
        , TagId
        , TagIdTag
        )
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Id exposing (IdDict, IdSet)
import List.Extra as List
import Router exposing (Route(..))
import Set.Any


type alias Model =
    { exerciseId : Maybe ExerciseId
    , name : String
    , sanskritName : String
    , image : String
    , description : String
    , positionId : Maybe PositionId
    , tags : IdSet TagIdTag
    }


type Msg
    = SetName String
    | SetSanskritName String
    | SetImage String
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
                                , sanskritName = emptyToNothing model.sanskritName
                                , image = emptyToNothing model.image
                                , description = model.description
                                , positionId = positionId
                                , tagIds = Set.Any.toList model.tags
                                }
            )


emptyToNothing : String -> Maybe String
emptyToNothing s =
    if String.isEmpty s then
        Nothing

    else
        Just s


initEditor : Exercise -> Model
initEditor exercise =
    { exerciseId = Just exercise.id
    , name = exercise.name
    , sanskritName = Maybe.withDefault "" exercise.sanskritName
    , image = Maybe.withDefault "" exercise.image
    , description = exercise.description
    , positionId = Just exercise.positionId
    , tags = Id.buildSet exercise.tagIds
    }


emptyEditor : Model
emptyEditor =
    { exerciseId = Nothing
    , name = ""
    , sanskritName = ""
    , image = ""
    , description = ""
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

        SetImage newImage ->
            ( { model | image = newImage }
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
    -> ImageVerificationResult
    -> Model
    -> Element Msg
viewEditor positions tags imageVerificationResults model =
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
        , Input.text [ fieldWidth ]
            { onChange = SetImage
            , text = model.image
            , placeholder = placeholder "Obrázek asana.png"
            , label = Input.labelHidden "Obrázek"
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
        , case
            model.exerciseId
                |> Maybe.andThen
                    (\exerciseId ->
                        Dict.Any.get exerciseId imageVerificationResults.invalidLinks
                    )
          of
            Just invalidImages ->
                E.el [ Font.color Color.orange ]
                    (E.text <| "Obkazované obrázky neexistují: " ++ String.join "," invalidImages)

            Nothing ->
                E.none
        , E.row [ E.spacing 30 ]
            [ positionRadios positions model.positionId
            , tagCheckboxes ToggleTagId 10 tags model.tags
            ]
        , E.row [ E.alignRight, E.spacing 5 ]
            [ E.link Common.coralButton
                { url =
                    case model.exerciseId of
                        Just exerciseId ->
                            Router.href (Router.Exercise exerciseId)

                        Nothing ->
                            Router.href Router.Exercises
                , label = E.text "Zrušit"
                }
            , Input.button Common.blueButton
                { onPress = Just SaveExercise
                , label = E.text "Uložit"
                }
            ]
        ]


positionRadios : IdDict PositionIdTag Position -> Maybe PositionId -> Element Msg
positionRadios positions maybeCurrentPosition =
    E.el
        [ {- Workaround - putting alignTop in radio's attributes doesn't align it -} E.alignTop ]
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
    E.column
        [ -- TODO figure out how to do this without enable vertical scrollbar without hardcoding height
          E.height (E.px 1000)
        , E.width (E.px 450)
        , E.alignTop
        ]
        [ Common.heading1 "Cviky"
        , createExerciseButton
        , Dict.Any.values exercises
            |> List.sortBy .name
            |> List.map exerciseMenuItem
            |> E.column [ E.scrollbarY, E.height E.fill ]
        ]


exerciseMenuItem : Exercise -> Element msg
exerciseMenuItem exercise =
    E.row
        [ Border.solid
        , Border.width 1
        , Border.color Color.lightGrey
        , E.width E.fill
        ]
        [ imagePreview exercise
        , E.column
            [ E.width E.fill
            , E.alignTop
            , E.padding 5
            , E.spacing 5
            ]
            [ E.paragraph [ E.width E.fill, Font.bold ]
                [ E.link []
                    { url = Router.href (Router.Exercise exercise.id)
                    , label = E.text exercise.name
                    }
                ]
            , case exercise.sanskritName of
                Just sanskritName ->
                    E.paragraph [ E.width E.fill, Font.italic ]
                        [ E.link []
                            { url = Router.href (Router.Exercise exercise.id)
                            , label = E.text sanskritName
                            }
                        ]

                Nothing ->
                    E.none
            ]
        ]


imagePreview : Exercise -> Element msg
imagePreview exercise =
    case exercise.image of
        Nothing ->
            E.el
                [ E.width (E.px previewImageSize)
                , E.height (E.px previewImageSize)
                , Background.color Color.lightGrey
                , Font.color Color.white
                ]
                (E.el [ E.centerX, E.centerY ]
                    (E.text "N/A")
                )

        Just image ->
            E.image
                [ E.width (E.px previewImageSize)
                , E.height (E.px previewImageSize)
                ]
                { src = image
                , description = exercise.name
                }


previewImageSize : Int
previewImageSize =
    100


createExerciseButton : Element msg
createExerciseButton =
    E.el [ E.paddingXY 0 5 ]
        (E.link Common.blueButton
            { url = Router.href (Router.ExerciseEditor Nothing)
            , label = E.text "Vytvořit cvik"
            }
        )


listWithDetail :
    Config msg
    -> IdDict PositionIdTag Position
    -> IdDict ExerciseIdTag Exercise
    -> IdDict TagIdTag Tag
    -> Maybe Exercise
    -> Element msg
listWithDetail config positions exercises tags maybeExercise =
    E.row
        [ E.spacing 10
        , E.height E.fill
        , E.width E.fill
        ]
        [ listView exercises
        , case maybeExercise of
            Just exercise ->
                E.el [ E.alignTop, E.width (E.fillPortion 2) ]
                    (exerciseDetail config positions tags exercises exercise)

            Nothing ->
                E.none
        ]


exerciseDetail :
    Config msg
    -> IdDict PositionIdTag Position
    -> IdDict TagIdTag Tag
    -> IdDict ExerciseIdTag Exercise
    -> Exercise
    -> Element msg
exerciseDetail config positions tags exercises exercise =
    E.column [ E.width E.fill, E.spacing 10 ]
        [ E.html imageSizeStyle
        , E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , Common.heading1 exercise.name
        , E.row [ E.spacing 5 ]
            [ E.link Common.blueButton
                { url = Router.href <| Router.ExerciseEditor <| Just exercise.id
                , label = E.text "Upravit"
                }
            , Input.button Common.coralButton
                { onPress = Just (config.deleteExercise exercise.id)
                , label = E.text "Odstranit"
                }
            ]
        , prevNextButtons exercises exercise
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
        , case exercise.image of
            Just imageFile ->
                E.image [ E.height <| E.maximum 350 E.shrink ]
                    { src = imageFile, description = imageFile }

            Nothing ->
                E.el
                    [ E.width (E.px 150)
                    , E.height (E.px 150)
                    , Background.color Color.lightGrey
                    , Font.color Color.white
                    ]
                    (E.el [ E.centerX, E.centerY ]
                        (E.text "- bez obrázku -")
                    )
        , E.paragraph []
            [ Common.markdown exercise.description ]
        , E.paragraph []
            [ E.link Common.linkAttrs
                { url = Router.href <| Router.Routines <| Just exercise.id
                , label = E.text "Sestavy obsahující tento cvik"
                }
            ]
        ]


prevNextButtons : IdDict ExerciseIdTag Exercise -> Exercise -> Element msg
prevNextButtons exercises exercise =
    E.row [ E.spacing 5 ]
        [ neighborExercise exercises exercise -1 "Předchozí"
        , neighborExercise exercises exercise 1 "Další"
        ]


neighborExercise :
    IdDict ExerciseIdTag Exercise
    -> Exercise
    -> Int
    -> String
    -> Element msg
neighborExercise es e diff label =
    let
        nameSortedExercises =
            Dict.Any.values es
                |> List.sortBy .name
    in
    nameSortedExercises
        |> List.findIndex (\e_ -> e_.id == e.id)
        |> Maybe.andThen (\thisIdx -> List.getAt (thisIdx + diff) nameSortedExercises)
        |> Maybe.map
            (\exercise ->
                E.link Common.blueButton
                    { url = Router.href <| Router.Exercise exercise.id
                    , label = E.text label
                    }
            )
        |> Maybe.withDefault E.none


imageSizeStyle : Html msg
imageSizeStyle =
    Html.node "style" [] [ Html.text ".image>img{height:300px;}" ]


backToList : Element msg
backToList =
    E.link Common.linkAttrs
        { url = Router.href Exercises
        , label = E.text "« Zavřít detail"
        }
