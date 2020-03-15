module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html
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
    { navKey : Key }


type Msg
    = NoOp
    | UrlRequest UrlRequest
    | UrlChange Url


init : () -> Url -> Key -> ( Model, Cmd msg )
init _ url key =
    ( { navKey = key }, Cmd.none )


view : Model -> Document msg
view model =
    { title = "sestavr", body = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequest urlReq ->
            ( model, Cmd.none )

        UrlChange url ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
