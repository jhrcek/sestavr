module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Events as Event
import Element.Font as Font
import Html exposing (Html)
import Router exposing (Route)
import Store exposing (Store)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


type alias Model =
    { navKey : Key
    , store : Store
    , route : Route
    , initialUrl : Url
    }


type Msg
    = UrlRequest UrlRequest
    | UrlChange Url
    | StoreMsg Store.Msg
    | SetRoute Route


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , store = Store.init
      , route = Router.parseUrl url
      , initialUrl = url
      }
    , Cmd.batch
        [ Store.getTargets StoreMsg
        ]
    )


view : Model -> Document Msg
view model =
    { title = "sestavr"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    viewLayout model.route <|
        case model.route of
            Router.Home ->
                E.text "Home"

            Router.Exercises ->
                E.text "Cviky"

            Router.Targets ->
                E.text <| Debug.toString model

            Router.NotFound what ->
                E.text <| "Tady nic není : " ++ what


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ initialUrl } as model) =
    case msg of
        StoreMsg storeMsg ->
            ( { model | store = Store.update storeMsg model.store }
            , Cmd.none
            )

        UrlChange url ->
            ( { model | route = Router.parseUrl url }
            , Cmd.none
            )

        SetRoute route ->
            ( model
            , navigateToRoute model.navKey model.initialUrl route
            )

        UrlRequest _ ->
            ( model, Cmd.none )


navigateToRoute : Key -> Url -> Route -> Cmd msg
navigateToRoute key initialUrl route =
    Browser.Navigation.pushUrl key <|
        Url.toString { initialUrl | fragment = Just <| Router.toHash route }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewLayout : Route -> Element Msg -> Html Msg
viewLayout route content =
    E.layout [] <|
        E.row
            [ E.width E.fill ]
            [ navigationLeft route
            , E.el [ E.width (E.fillPortion 10) ] content
            ]


navigationLeft : Route -> Element Msg
navigationLeft currentRoute =
    E.column [ E.height E.fill, E.width (E.fillPortion 1) ]
        [ menuItem currentRoute Router.Home "Domů"
        , menuItem currentRoute Router.Targets "Cílové partie"
        , menuItem currentRoute Router.Exercises "Cviky"
        ]


menuItem : Route -> Route -> String -> Element Msg
menuItem currentRoute targetRoute title =
    E.el
        [ Background.color <|
            if targetRoute == currentRoute then
                E.rgb255 17 117 183

            else
                E.rgb255 1 5 121
        , Font.color (E.rgb255 255 255 255)
        , E.width E.fill
        , E.padding 30
        , Event.onClick <| SetRoute targetRoute
        ]
        (E.text title)
