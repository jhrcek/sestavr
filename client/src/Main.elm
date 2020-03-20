module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict.Any
import Domain exposing (ExerciseId, Target, TargetId)
import Element as E exposing (Element)
import Element.Background as Background
import Element.Events as Event
import Element.Font as Font
import Http.Extra as Ht2
import Id
import Modal
import Page.Exercise as Exercise
import Page.Targets as Targets
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
    , pageModel : PageModel
    , httpError : Maybe Ht2.Error
    }


initPage : Route -> Store -> PageModel
initPage route store =
    case route of
        Router.Targets ->
            TargetsModel Targets.init

        Router.Home ->
            HomeModel

        Router.Exercises ->
            ExercisesModel

        Router.Exercise exerciseId ->
            ExerciseModel exerciseId

        Router.ExerciseEditor exerciseId ->
            ExerciseEditor <|
                case Dict.Any.get exerciseId store.exercises of
                    Just exercise ->
                        Exercise.initEditor exercise

                    Nothing ->
                        Exercise.emptyEditor

        Router.NotFound what ->
            NotFoundModel what


type PageModel
    = TargetsModel Targets.Model
    | HomeModel
    | ExercisesModel
    | ExerciseModel ExerciseId
    | ExerciseEditor Exercise.Model
    | NotFoundModel String


type Msg
    = UrlRequest UrlRequest
    | UrlChange Url
    | StoreMsg Store.Msg
    | SetRoute Route
    | TargetsMsg Targets.Msg
    | CreateTarget Target
    | DeleteTarget TargetId
    | UpdateTarget Target
    | ErrorAcked


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Router.parseUrl url
    in
    ( { navKey = key
      , store = Store.init
      , route = route
      , initialUrl = url
      , pageModel = initPage route Store.init
      , httpError = Nothing
      }
    , Cmd.map StoreMsg <|
        Cmd.batch
            [ Store.getTargets
            , Store.getExercises
            ]
    )


view : Model -> Document Msg
view model =
    { title = "sestavr"
    , body =
        [ let
            modal =
                case model.httpError of
                    Just error ->
                        [ E.inFront <| Modal.viewError ErrorAcked error ]

                    Nothing ->
                        []
          in
          E.layout modal (viewBody model)
        ]
    }


viewBody : Model -> Element Msg
viewBody model =
    viewLayout model.route <|
        case model.pageModel of
            HomeModel ->
                E.text "Home"

            ExercisesModel ->
                Exercise.viewList model.store.exercises

            ExerciseModel exerciseId ->
                case Dict.Any.get exerciseId model.store.exercises of
                    Just exercise ->
                        Exercise.view exercise

                    Nothing ->
                        E.text <| "Cvičení s ID " ++ Id.toString exerciseId ++ " neexistuje"

            ExerciseEditor emodel ->
                Exercise.viewEditor emodel

            TargetsModel tmodel ->
                E.map TargetsMsg <| Targets.view model.store.targets tmodel

            NotFoundModel what ->
                E.text <| "Tady nic není : " ++ what


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoreMsg storeMsg ->
            let
                ( newStore, maybeError ) =
                    Store.update storeMsg model.store
            in
            ( { model
                | store = newStore
                , httpError = maybeError
              }
            , Cmd.none
            )

        UrlChange url ->
            let
                newRoute =
                    Router.parseUrl url
            in
            ( { model
                | route = newRoute
                , pageModel = initPage newRoute model.store
              }
            , Cmd.none
            )

        SetRoute route ->
            ( model
            , navigateToRoute model.navKey model.initialUrl route
            )

        UrlRequest urlRequest ->
            ( model
            , case urlRequest of
                Browser.Internal url ->
                    Nav.pushUrl model.navKey (Url.toString url)

                Browser.External url ->
                    Nav.load url
            )

        TargetsMsg targetsMsg ->
            let
                ( newPageModel, targetCmd ) =
                    case model.pageModel of
                        TargetsModel tm ->
                            Tuple.mapFirst TargetsModel <|
                                Targets.update targetConfig targetsMsg tm

                        x ->
                            ( x, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , targetCmd
            )

        CreateTarget target ->
            ( model
            , Cmd.map StoreMsg <| Store.createTarget target
            )

        DeleteTarget targetId ->
            ( model
            , Cmd.map StoreMsg <| Store.deleteTarget targetId
            )

        ErrorAcked ->
            ( { model | httpError = Nothing }
            , Cmd.none
            )

        UpdateTarget target ->
            ( model
            , Cmd.map StoreMsg <| Store.updateTarget target
            )


targetConfig : Targets.Config Msg
targetConfig =
    { createTarget = CreateTarget
    , deleteTarget = DeleteTarget
    , updateTarget = UpdateTarget
    }


navigateToRoute : Key -> Url -> Route -> Cmd msg
navigateToRoute key initialUrl route =
    Nav.pushUrl key <|
        Url.toString { initialUrl | fragment = Just <| Router.toHash route }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


viewLayout : Route -> Element Msg -> Element Msg
viewLayout route content =
    E.row
        [ E.width E.fill ]
        [ navigationLeft route
        , E.el
            [ E.alignTop
            , E.padding 50
            ]
            content
        ]


navigationLeft : Route -> Element Msg
navigationLeft currentRoute =
    E.column [ E.height E.fill, E.width (E.px 180) ]
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
