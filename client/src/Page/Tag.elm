module Page.Tag exposing
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
import Domain exposing (Tag, TagId, TagIdTag)
import Element as E exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Id exposing (IdDict)
import Task


type alias Config msg =
    { createTag : Tag -> msg
    , deleteTag : TagId -> msg
    , updateTag : Tag -> msg
    , noop : msg
    }


type Msg
    = AddClicked
    | CancelClicked
    | EditClicked Tag
    | EditedNameChanged String
    | SaveEditedNameClicked
    | SaveNewNameClicked
    | NewNameChanged String
    | DeleteClicked TagId


type alias Model =
    { newField : Maybe String
    , editedTag : Maybe Tag
    }


init : Model
init =
    { newField = Nothing
    , editedTag = Nothing
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        AddClicked ->
            ( { model | newField = Just "" }
            , Dom.focus newTagInputId |> Task.attempt (always config.noop)
            )

        NewNameChanged newName ->
            ( { model | newField = Maybe.map (always newName) model.newField }
            , Cmd.none
            )

        CancelClicked ->
            ( { model | newField = Nothing }
            , Cmd.none
            )

        EditClicked tag ->
            ( { model | editedTag = Just tag }
            , Cmd.none
            )

        EditedNameChanged newName ->
            ( { model | editedTag = Maybe.map (\t -> { t | name = newName }) model.editedTag }
            , Cmd.none
            )

        SaveNewNameClicked ->
            let
                createTag =
                    case model.newField of
                        Just tagName ->
                            Command.perform <|
                                config.createTag
                                    { id = Id.fromInt 0, name = tagName }

                        Nothing ->
                            Cmd.none
            in
            ( { model | newField = Nothing }
            , createTag
            )

        SaveEditedNameClicked ->
            let
                updateTag =
                    case model.editedTag of
                        Just tag ->
                            Command.perform <| config.updateTag tag

                        Nothing ->
                            Cmd.none
            in
            ( { model | editedTag = Nothing }, updateTag )

        DeleteClicked tagId ->
            ( model, Command.perform <| config.deleteTag tagId )


view : IdDict TagIdTag Tag -> Model -> Element Msg
view tags model =
    E.column [ E.width (E.maximum 500 <| E.px 300) ]
        [ Common.heading1 "Tagy"
        , form model
        , viewTags tags model.editedTag
        ]


form : Model -> Element Msg
form model =
    case model.newField of
        Nothing ->
            E.el [ E.paddingXY 0 5 ]
                (Input.button Common.buttonAttrs
                    { onPress = Just AddClicked
                    , label = E.text "Vytvořit tag"
                    }
                )

        Just fieldName ->
            E.column [ E.paddingXY 0 5 ]
                [ E.text "Tvorba nového tagu"
                , Input.text
                    [ E.htmlAttribute (Html.Attributes.id newTagInputId)
                    , E.width (E.px 100)
                    ]
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


viewTags : IdDict TagIdTag Tag -> Maybe Tag -> Element Msg
viewTags tags maybeEdited =
    let
        colHeader label =
            E.el
                [ Border.solid
                , Border.width 1
                , Border.color Color.lightGrey
                , E.padding 5
                ]
                (E.text label)
    in
    E.table
        [ Border.solid
        , Border.width 1
        , Border.color Color.lightGrey
        ]
        { data = List.sortBy .name <| Dict.Any.values tags
        , columns =
            [ { header = colHeader "Název"
              , width = E.fill
              , view =
                    \tag ->
                        case maybeEdited of
                            Just editedTag ->
                                if tag.id == editedTag.id then
                                    Input.text [ E.width (E.px 100), E.height (E.px 30), E.padding 5 ]
                                        { onChange = EditedNameChanged
                                        , text = editedTag.name
                                        , placeholder = Nothing
                                        , label = Input.labelHidden "Název"
                                        }

                                else
                                    E.el
                                        [ Border.solid
                                        , Border.width 1
                                        , Border.color Color.lightGrey
                                        , E.padding 5
                                        ]
                                        (E.text tag.name)

                            Nothing ->
                                E.el
                                    [ Border.solid
                                    , Border.width 1
                                    , Border.color Color.lightGrey
                                    , E.padding 5
                                    ]
                                    (E.text tag.name)
              }
            , { header = colHeader "Možnosti"
              , width = E.fill
              , view =
                    \tag ->
                        E.row [ E.spacing 2 ]
                            [ Common.iconButton <|
                                case maybeEdited of
                                    Just editedTag ->
                                        if tag.id == editedTag.id then
                                            { onPress = Just SaveEditedNameClicked, label = E.text "💾" }

                                        else
                                            { onPress = Just (EditClicked tag), label = E.text "🖉" }

                                    Nothing ->
                                        { onPress = Just (EditClicked tag), label = E.text "🖉" }
                            , Common.iconButton { onPress = Just (DeleteClicked tag.id), label = E.text "🗑" }
                            ]
              }
            ]
        }


newTagInputId : String
newTagInputId =
    "tag-input"
