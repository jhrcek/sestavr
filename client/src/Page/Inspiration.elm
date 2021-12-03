module Page.Inspiration exposing
    ( Config
    , Model
    , Msg
    , init
    , update
    , view
    )

import Command
import Common
import Dict.Any
import Domain exposing (Inspiration, InspirationId, InspirationIdTag)
import Element as E exposing (Element)
import Element.Border as Border
import Element.Events as Event
import Element.Input as Input
import Id exposing (IdDict)
import List.Extra as List
import Time exposing (Posix)
import Time.Extra as Time


type alias Config msg =
    { updateInspiration : Inspiration -> msg
    }


type alias Model =
    { editedInspiration : Maybe Inspiration
    , monthToggles : IdDict InspirationIdTag Bool
    , currentMonth : InspirationId
    }


type Msg
    = SetDescription String
    | EditInspiration Inspiration
    | ToggleMonth InspirationId
    | CancelEdit
    | SaveEdit


init : Posix -> Model
init today =
    let
        currentMonth =
            Time.monthToInt <| Time.toMonth Time.utc today
    in
    { editedInspiration = Nothing
    , currentMonth = Id.fromInt currentMonth
    , monthToggles =
        List.range 1 12
            |> List.map (\monthNum -> ( Id.fromInt monthNum, monthNum == currentMonth ))
            |> Id.dictFromList
    }


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

        ToggleMonth inspirationId ->
            ( { model | monthToggles = Dict.Any.update inspirationId (Maybe.map not) model.monthToggles }
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
    -- Rotate inspiration months so that current month is at the top
    let
        rotatedInspirations =
            case
                Dict.Any.values inspirations
                    |> List.splitWhen (\inspiration -> inspiration.id == model.currentMonth)
            of
                Just ( before, after ) ->
                    after ++ before

                Nothing ->
                    []
    in
    E.column [ E.width E.fill ]
        [ Common.heading1 "Inspirace"
        , rotatedInspirations
            |> List.map
                (\i ->
                    let
                        isOpened =
                            Dict.Any.get i.id model.monthToggles |> Maybe.withDefault False
                    in
                    case model.editedInspiration of
                        Nothing ->
                            viewInspiration isOpened i

                        Just editedInspiration ->
                            if i.id == editedInspiration.id then
                                inspirationEditor editedInspiration

                            else
                                viewInspiration isOpened i
                )
            |> E.column
                [ E.width E.fill
                , E.spacing 10
                ]
        ]


viewInspiration : Bool -> Inspiration -> Element Msg
viewInspiration isOpened inspiration =
    let
        monthTitle =
            (if isOpened then
                " ▾ "

             else
                " ▸ "
            )
                ++ Time.toCzechMonth (Time.monthFromNumber inspiration.monthNumber)

        heading =
            E.el
                [ E.width E.fill
                , Event.onClick <| ToggleMonth inspiration.id
                ]
                (Common.heading2 monthTitle)

        body =
            if isOpened then
                [ E.paragraph []
                    [ Common.markdown inspiration.description ]
                , Input.button Common.blueButton
                    { onPress = Just (EditInspiration inspiration)
                    , label = E.text "Upravit"
                    }
                ]

            else
                []
    in
    inspirationColumn (heading :: body)


inspirationEditor : Inspiration -> Element Msg
inspirationEditor inspiration =
    inspirationColumn
        [ Common.heading2 <| Time.toCzechMonth <| Time.monthFromNumber inspiration.monthNumber
        , Input.multiline
            [ E.width E.fill
            , E.height
                (E.fill
                    |> E.minimum 400
                    |> E.maximum 600
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
        , Border.rounded 5
        , E.padding 5
        , E.spacing 5
        ]
