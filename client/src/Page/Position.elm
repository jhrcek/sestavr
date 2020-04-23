module Page.Position exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    , view
    )

import Color
import Command
import Common
import Dict.Any
import Domain exposing (Position, PositionId, PositionIdTag)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Id exposing (IdDict)


type alias Config msg =
    { createPosition : Position -> msg
    , deletePosition : PositionId -> msg
    , updatePosition : Position -> msg
    }


type Msg
    = AddClicked
    | CancelClicked
    | EditClicked Position
    | EditedNameChanged String
    | SaveEditedNameClicked
    | SaveNewNameClicked
    | NewNameChanged String
    | DeleteClicked PositionId


type alias Model =
    { newField : Maybe String
    , editedPosition : Maybe Position
    }


init : Model
init =
    { newField = Nothing
    , editedPosition = Nothing
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        AddClicked ->
            ( { model | newField = Just "" }
            , Cmd.none
            )

        NewNameChanged newName ->
            ( { model | newField = Maybe.map (always newName) model.newField }
            , Cmd.none
            )

        CancelClicked ->
            ( { model | newField = Nothing }
            , Cmd.none
            )

        EditClicked target ->
            ( { model | editedPosition = Just target }
            , Cmd.none
            )

        EditedNameChanged newName ->
            ( { model | editedPosition = Maybe.map (\t -> { t | name = newName }) model.editedPosition }
            , Cmd.none
            )

        SaveNewNameClicked ->
            let
                createPosition =
                    case model.newField of
                        Just targetName ->
                            Command.perform <|
                                config.createPosition
                                    { id = Id.fromInt 0, name = targetName }

                        Nothing ->
                            Cmd.none
            in
            ( { model | newField = Nothing }
            , createPosition
            )

        SaveEditedNameClicked ->
            let
                updatePosition =
                    case model.editedPosition of
                        Just target ->
                            Command.perform <| config.updatePosition target

                        Nothing ->
                            Cmd.none
            in
            ( { model | editedPosition = Nothing }, updatePosition )

        DeleteClicked targetId ->
            ( model, Command.perform <| config.deletePosition targetId )


view : IdDict PositionIdTag Position -> Model -> Element Msg
view targets model =
    E.column [ E.width (E.maximum 500 <| E.px 300) ]
        [ Common.heading1 "Pozice"
        , viewPositions targets model.editedPosition
        , form model
        ]


form : Model -> Element Msg
form model =
    case model.newField of
        Nothing ->
            Input.button Common.buttonAttrs
                { onPress = Just AddClicked
                , label = E.text "Přidat pozici"
                }

        Just fieldName ->
            E.column []
                [ E.text "Tvorba nové pozice"
                , Input.text [ E.width (E.px 100) ]
                    { onChange = NewNameChanged
                    , text = fieldName
                    , placeholder = Nothing
                    , label = Input.labelLeft [ E.centerY ] (E.text "Název")
                    }
                , E.row [ E.spacing 5, E.padding 5 ]
                    [ Input.button Common.buttonAttrs
                        { onPress = Just CancelClicked
                        , label = E.text "Zrušit"
                        }
                    , Input.button Common.buttonAttrs
                        { onPress =
                            if String.isEmpty fieldName then
                                Nothing

                            else
                                Just SaveNewNameClicked
                        , label = E.text "Uložit"
                        }
                    ]
                ]


viewPositions : IdDict PositionIdTag Position -> Maybe Position -> Element Msg
viewPositions targets maybeEdited =
    let
        iconButton =
            Input.button
                [ Border.solid
                , Border.width 1
                , E.padding 5
                , Border.rounded 4
                , E.mouseOver [ Background.color Color.lightGrey ]
                ]

        colHeader label =
            E.el
                [ Border.solid
                , Border.width 1
                , E.padding 5
                , Background.color Color.lightGrey
                ]
                (E.text label)
    in
    E.table
        [ Border.solid
        , Border.width 1
        , E.spacing 2
        , E.padding 2
        ]
        { data = List.sortBy .name <| Dict.Any.values targets
        , columns =
            [ { header = colHeader "Název"
              , width = E.fill
              , view =
                    \target ->
                        case maybeEdited of
                            Just editedPosition ->
                                if target.id == editedPosition.id then
                                    Input.text [ E.width (E.px 100), E.height (E.px 30), E.padding 4 ]
                                        { onChange = EditedNameChanged
                                        , text = editedPosition.name
                                        , placeholder = Nothing
                                        , label = Input.labelHidden "Název"
                                        }

                                else
                                    E.el
                                        [ Border.solid
                                        , Border.width 1
                                        , E.padding 5
                                        ]
                                        (E.text target.name)

                            Nothing ->
                                E.el
                                    [ Border.solid
                                    , Border.width 1
                                    , E.padding 5
                                    ]
                                    (E.text target.name)
              }
            , { header = colHeader "Možnosti"
              , width = E.fill
              , view =
                    \target ->
                        E.row [ E.spacing 2 ]
                            [ iconButton <|
                                case maybeEdited of
                                    Just editedPosition ->
                                        if target.id == editedPosition.id then
                                            { onPress = Just SaveEditedNameClicked, label = E.text "💾" }

                                        else
                                            { onPress = Just (EditClicked target), label = E.text "🖉" }

                                    Nothing ->
                                        { onPress = Just (EditClicked target), label = E.text "🖉" }
                            , iconButton { onPress = Just (DeleteClicked target.id), label = E.text "🗑" }
                            ]
              }

            -- TODO show # of exercises for each position
            -- TODO link to the search of exercises that have given position
            ]
        }
