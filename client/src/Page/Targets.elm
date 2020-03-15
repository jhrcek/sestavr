module Page.Targets exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    , view
    )

import Command
import Dict.Any
import Domain exposing (Target, TargetIdTag)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Id exposing (IdDict)


type alias Config msg =
    { createTarget : Target -> msg
    , msg : Msg -> msg
    }


type Msg
    = AddClicked
    | CancelClicked
    | SaveClicked
    | NameChanged String


type alias Model =
    { newField : Maybe String
    }


init : Model
init =
    { newField = Nothing }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        AddClicked ->
            ( { newField = Just "" }
            , Cmd.none
            )

        NameChanged newName ->
            ( { model | newField = Maybe.map (always newName) model.newField }
            , Cmd.none
            )

        CancelClicked ->
            ( { model | newField = Nothing }
            , Cmd.none
            )

        SaveClicked ->
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


view : Config msg -> IdDict TargetIdTag Target -> Model -> Element msg
view config targets model =
    E.column []
        [ viewTargets targets
        , form config model
        ]


form : Config msg -> Model -> Element msg
form config model =
    case model.newField of
        Nothing ->
            button config
                { onPress = Just AddClicked
                , label = E.text "Přidat partii"
                }

        Just fieldName ->
            E.column []
                [ Input.text [ E.width (E.px 100) ]
                    { onChange = config.msg << NameChanged
                    , text = fieldName
                    , placeholder = Nothing
                    , label = Input.labelLeft [ E.centerY ] (E.text "Název")
                    }
                , E.row []
                    [ button config
                        { onPress =
                            if String.isEmpty fieldName then
                                Nothing

                            else
                                Just SaveClicked
                        , label = E.text "Uložit"
                        }
                    , button config
                        { onPress = Just CancelClicked
                        , label = E.text "Zrušit"
                        }
                    ]
                ]


button : Config msg -> { onPress : Maybe Msg, label : Element Msg } -> Element msg
button config =
    E.map config.msg << Input.button [ Border.solid, Border.width 2, E.padding 5, Border.rounded 4 ]


viewTargets : IdDict TargetIdTag Target -> Element msg
viewTargets targets =
    E.table
        [ Border.solid
        , Border.width 1
        , E.spacing 2
        , E.padding 2
        ]
        { data = List.sortBy .name <| Dict.Any.values targets
        , columns =
            [ { header =
                    E.el
                        [ Border.color (E.rgb255 0 0 0)
                        , Border.solid
                        , Border.width 1
                        , E.padding 5
                        , Background.color (E.rgb255 195 195 195)
                        ]
                        (E.text "Název")
              , width = E.fill
              , view =
                    \target ->
                        E.el
                            [ Border.color (E.rgb255 0 0 0)
                            , Border.solid
                            , Border.width 1
                            , E.padding 5
                            ]
                            (E.text target.name)
              }
            ]
        }
