module Page.Inspiration exposing (Config, Model, Msg, init, update, view)

import Command
import Common
import Dict.Any
import Domain exposing (Inspiration, InspirationIdTag)
import Element as E exposing (Element)
import Element.Border as Border
import Element.Input as Input
import Id exposing (IdDict)
import Time.Extra as Time


type alias Config msg =
    { updateInspiration : Inspiration -> msg
    }


type alias Model =
    { editedInspiration : Maybe Inspiration
    }


type Msg
    = SetDescription String
    | EditInspiration Inspiration
    | CancelEdit
    | SaveEdit


init : Model
init =
    { editedInspiration = Nothing }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        SetDescription newDesc ->
            ( { model | editedInspiration = Maybe.map (setDescription newDesc) model.editedInspiration }
            , Cmd.none
            )

        EditInspiration inspiration ->
            ( { model | editedInspiration = Just inspiration }, Cmd.none )

        CancelEdit ->
            ( { model | editedInspiration = Nothing }
            , Cmd.none
            )

        SaveEdit ->
            let
                updateCmd =
                    case model.editedInspiration of
                        Just inspiration ->
                            Command.perform <| config.updateInspiration inspiration

                        Nothing ->
                            Cmd.none
            in
            ( { model | editedInspiration = Nothing }
            , updateCmd
            )


setDescription : String -> Inspiration -> Inspiration
setDescription description inspiration =
    { inspiration | description = description }


view : IdDict InspirationIdTag Inspiration -> Model -> Element Msg
view inspirations model =
    E.column [ E.width E.fill ]
        [ Common.heading1 "Inspirace"
        , Dict.Any.values inspirations
            |> List.map
                (\i ->
                    case model.editedInspiration of
                        Nothing ->
                            viewInspiration i

                        Just editedInspiration ->
                            if i.id == editedInspiration.id then
                                inspirationEditor editedInspiration

                            else
                                viewInspiration i
                )
            |> E.column
                [ E.width E.fill
                , E.spacing 10
                ]
        ]


viewInspiration : Inspiration -> Element Msg
viewInspiration inspiration =
    inspirationColumn
        [ Common.heading2 <| Time.toCzechMonth <| Time.monthFromNumber inspiration.monthNumber
        , E.paragraph []
            [ Common.markdown inspiration.description ]
        , Input.button Common.blueButton
            { onPress = Just (EditInspiration inspiration)
            , label = E.text "Upravit"
            }
        ]


inspirationEditor : Inspiration -> Element Msg
inspirationEditor inspiration =
    inspirationColumn
        [ Common.heading2 <| Time.toCzechMonth <| Time.monthFromNumber inspiration.monthNumber
        , Input.multiline
            [ E.width E.fill
            , E.height
                (E.fill
                    |> E.minimum 200
                    |> E.maximum 400
                )
            ]
            { onChange = SetDescription
            , text = inspiration.description
            , placeholder = Just (Input.placeholder [] (E.text "Text inspirace (markdown)"))
            , spellcheck = False
            , label = Input.labelHidden "text inspirace"
            }
        , E.row [ E.spacing 5, E.paddingXY 0 5 ]
            [ Input.button Common.coralButton
                { onPress = Just CancelEdit
                , label = E.text "Zrušit"
                }
            , Input.button Common.blueButton
                { onPress = Just SaveEdit
                , label = E.text "Uložit"
                }
            ]
        ]


inspirationColumn : List (Element msg) -> Element msg
inspirationColumn =
    E.column
        [ E.width E.fill
        , Border.width 1
        , E.padding 5
        , E.spacing 5
        ]
