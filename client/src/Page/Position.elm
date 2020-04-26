module Page.Position exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    , view
    )

import Browser.Dom as Dom
import Color
import Command
import Common
import Dict.Any
import Domain exposing (Position, PositionId, PositionIdTag)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Id exposing (IdDict)
import Task


type alias Config msg =
    { createPosition : Position -> msg
    , deletePosition : PositionId -> msg
    , updatePosition : Position -> msg
    , noop : msg
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
            , Dom.focus newPositionInputId |> Task.attempt (always config.noop)
            )

        NewNameChanged newName ->
            ( { model | newField = Maybe.map (always newName) model.newField }
            , Cmd.none
            )

        CancelClicked ->
            ( { model | newField = Nothing }
            , Cmd.none
            )

        EditClicked position ->
            ( { model | editedPosition = Just position }
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
                        Just positionName ->
                            Command.perform <|
                                config.createPosition
                                    { id = Id.fromInt 0, name = positionName }

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
                        Just position ->
                            Command.perform <| config.updatePosition position

                        Nothing ->
                            Cmd.none
            in
            ( { model | editedPosition = Nothing }, updatePosition )

        DeleteClicked positionId ->
            ( model, Command.perform <| config.deletePosition positionId )


view : IdDict PositionIdTag Position -> Model -> Element Msg
view positions model =
    E.column [ E.width (E.maximum 500 <| E.px 300) ]
        [ Common.heading1 "Pozice"
        , viewPositions positions model.editedPosition
        , form model
        ]


form : Model -> Element Msg
form model =
    case model.newField of
        Nothing ->
            E.el [ E.paddingXY 0 5 ]
                (Input.button Common.buttonAttrs
                    { onPress = Just AddClicked
                    , label = E.text "Vytvořit pozici"
                    }
                )

        Just fieldName ->
            E.column [ E.paddingXY 0 5 ]
                [ E.text "Tvorba nové pozice"
                , Input.text
                    [ E.htmlAttribute (Html.Attributes.id newPositionInputId)
                    , E.width (E.px 100)
                    ]
                    { onChange = NewNameChanged
                    , text = fieldName
                    , placeholder = Nothing
                    , label = Input.labelLeft [ E.centerY ] (E.text "Název")
                    }
                , E.row [ E.spacing 5, E.paddingXY 0 5 ]
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
viewPositions positions maybeEdited =
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
        { data = List.sortBy .name <| Dict.Any.values positions
        , columns =
            [ { header = colHeader "Název"
              , width = E.fill
              , view =
                    \position ->
                        case maybeEdited of
                            Just editedPosition ->
                                if position.id == editedPosition.id then
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
                                        (E.text position.name)

                            Nothing ->
                                E.el
                                    [ Border.solid
                                    , Border.width 1
                                    , E.padding 5
                                    ]
                                    (E.text position.name)
              }
            , { header = colHeader "Možnosti"
              , width = E.fill
              , view =
                    \position ->
                        E.row [ E.spacing 2 ]
                            [ iconButton <|
                                case maybeEdited of
                                    Just editedPosition ->
                                        if position.id == editedPosition.id then
                                            { onPress = Just SaveEditedNameClicked, label = E.text "💾" }

                                        else
                                            { onPress = Just (EditClicked position), label = E.text "🖉" }

                                    Nothing ->
                                        { onPress = Just (EditClicked position), label = E.text "🖉" }
                            , iconButton { onPress = Just (DeleteClicked position.id), label = E.text "🗑" }
                            ]
              }
            ]
        }


newPositionInputId : String
newPositionInputId =
    "position-input"
