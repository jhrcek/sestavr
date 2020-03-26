module Page.Target exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    , view
    )

import Command
import Dict.Any
import Domain exposing (Target, TargetId, TargetIdTag)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Id exposing (IdDict)


type alias Config msg =
    { createTarget : Target -> msg
    , deleteTarget : TargetId -> msg
    , updateTarget : Target -> msg
    }


type Msg
    = AddClicked
    | CancelClicked
    | EditClicked Target
    | CancelEditClicked
    | EditedNameChanged String
    | SaveEditedNameClicked
    | SaveNewNameClicked
    | NewNameChanged String
    | DeleteClicked TargetId


type alias Model =
    { newField : Maybe String
    , editedTarget : Maybe Target
    }


init : Model
init =
    { newField = Nothing
    , editedTarget = Nothing
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
            ( { model | editedTarget = Just target }
            , Cmd.none
            )

        CancelEditClicked ->
            ( { model | editedTarget = Nothing }, Cmd.none )

        EditedNameChanged newName ->
            ( { model | editedTarget = Maybe.map (\t -> { t | name = newName }) model.editedTarget }
            , Cmd.none
            )

        SaveNewNameClicked ->
            let
                createTarget =
                    case model.newField of
                        Just targetName ->
                            Command.perform <|
                                config.createTarget
                                    { id = Id.fromInt 0, name = targetName }

                        Nothing ->
                            Cmd.none
            in
            ( { model | newField = Nothing }
            , createTarget
            )

        SaveEditedNameClicked ->
            let
                updateTarget =
                    case model.editedTarget of
                        Just target ->
                            Command.perform <| config.updateTarget target

                        Nothing ->
                            Cmd.none
            in
            ( { model | editedTarget = Nothing }, updateTarget )

        DeleteClicked targetId ->
            ( model, Command.perform <| config.deleteTarget targetId )


view : IdDict TargetIdTag Target -> Model -> Element Msg
view targets model =
    E.column [ E.width (E.maximum 500 <| E.px 300) ]
        [ viewTargets targets model.editedTarget
        , form model
        ]


form : Model -> Element Msg
form model =
    case model.newField of
        Nothing ->
            button
                { onPress = Just AddClicked
                , label = E.text "Přidat partii"
                }

        Just fieldName ->
            E.column []
                [ E.text "Tvorba nové partie"
                , Input.text [ E.width (E.px 100) ]
                    { onChange = NewNameChanged
                    , text = fieldName
                    , placeholder = Nothing
                    , label = Input.labelLeft [ E.centerY ] (E.text "Název")
                    }
                , E.row [ E.spacing 5, E.padding 5 ]
                    [ button
                        { onPress = Just CancelClicked
                        , label = E.text "Zrušit"
                        }
                    , button
                        { onPress =
                            if String.isEmpty fieldName then
                                Nothing

                            else
                                Just SaveNewNameClicked
                        , label = E.text "Uložit"
                        }
                    ]
                ]


button : { onPress : Maybe Msg, label : Element Msg } -> Element Msg
button =
    Input.button
        [ Border.solid
        , Border.width 1
        , E.padding 5
        , Border.rounded 4
        ]


viewTargets : IdDict TargetIdTag Target -> Maybe Target -> Element Msg
viewTargets targets maybeEdited =
    let
        iconButton =
            Input.button
                [ Border.solid
                , Border.width 1
                , E.padding 5
                , Border.rounded 4
                , E.mouseOver [ Background.color (E.rgb255 192 192 192) ]
                ]

        colHeader label =
            E.el
                [ Border.solid
                , Border.width 1
                , E.padding 5
                , Background.color (E.rgb255 192 192 192)
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
                            Just editedTarget ->
                                if target.id == editedTarget.id then
                                    Input.text [ E.width (E.px 100), E.height (E.px 30), E.padding 5 ]
                                        { onChange = EditedNameChanged
                                        , text = editedTarget.name
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
                                    Just editedTarget ->
                                        if target.id == editedTarget.id then
                                            { onPress = Just SaveEditedNameClicked, label = E.text "💾" }

                                        else
                                            { onPress = Just (EditClicked target), label = E.text "🖉" }

                                    Nothing ->
                                        { onPress = Just (EditClicked target), label = E.text "🖉" }

                            -- TODO user confirmation before deletion
                            -- TODO don't allow deletion if used in some exercise
                            , iconButton { onPress = Just (DeleteClicked target.id), label = E.text "🗑" }
                            ]
              }

            -- TODO show # of exercises for each target
            -- TODO link to the search of exercises that have given target
            ]
        }
