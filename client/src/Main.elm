module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Dict.Any
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , Position
        , PositionId
        , Target
        , TargetId
        )
import Element as E exposing (Element)
import Element.Background as Background
import Element.Events as Event
import Element.Font as Font
import Http.Extra as Ht2
import Id
import Modal
import Page.Exercise as Exercise
import Page.Position as Position
import Page.Target as Target
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
        Router.Home ->
            HomeModel

        Router.Positions ->
            PositionModel Position.init

        Router.Targets ->
            TargetModel Target.init

        Router.Exercises ->
            ExercisesModel

        Router.Exercise exerciseId ->
            ExerciseModel exerciseId

        Router.ExerciseEditor maybeExerciseId ->
            maybeExerciseId
                |> Maybe.andThen (\exerciseId -> Dict.Any.get exerciseId store.exercises)
                |> Maybe.map (\exercise -> Exercise.initEditor exercise)
                |> Maybe.withDefault Exercise.emptyEditor
                |> ExerciseEditor

        Router.NotFound what ->
            NotFoundModel what


type PageModel
    = HomeModel
      -- Exercise
    | ExercisesModel
    | ExerciseModel ExerciseId
    | ExerciseEditor Exercise.Model
      -- Target
    | TargetModel Target.Model
      -- Position
    | PositionModel Position.Model
    | NotFoundModel String


type Msg
    = UrlRequest UrlRequest
    | UrlChange Url
    | StoreMsg Store.Msg
    | SetRoute Route
    | ExerciseMsg Exercise.Msg
      -- Target
    | TargetMsg Target.Msg
    | CreateTarget Target
    | DeleteTarget TargetId
    | UpdateTarget Target
      -- Position
    | PositionMsg Position.Msg
    | CreatePosition Position
    | DeletePosition PositionId
    | UpdatePosition Position
      -- Exercise
    | UpdateExercise Exercise (List TargetId)
    | CreateExercise Exercise (List TargetId)
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
            , Store.getPositions
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
                        Exercise.view
                            model.store.positions
                            model.store.targets
                            exercise

                    Nothing ->
                        E.text <| "Cvičení s ID " ++ Id.toString exerciseId ++ " neexistuje"

            ExerciseEditor emodel ->
                E.map ExerciseMsg <|
                    Exercise.viewEditor
                        model.store.positions
                        model.store.targets
                        emodel

            TargetModel tmodel ->
                E.map TargetMsg <| Target.view model.store.targets tmodel

            PositionModel pmodel ->
                E.map PositionMsg <| Position.view model.store.positions pmodel

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

        TargetMsg targetMsg ->
            let
                ( newPageModel, targetCmd ) =
                    case model.pageModel of
                        TargetModel m ->
                            Tuple.mapFirst TargetModel <|
                                Target.update targetConfig targetMsg m

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , targetCmd
            )

        PositionMsg positionMsg ->
            let
                ( newPageModel, positionCmd ) =
                    case model.pageModel of
                        PositionModel m ->
                            Tuple.mapFirst PositionModel <|
                                Position.update positionConfig positionMsg m

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , positionCmd
            )

        ExerciseMsg exerciseMsg ->
            let
                ( newPageModel, exerciseCmd ) =
                    case model.pageModel of
                        ExerciseEditor m ->
                            Tuple.mapFirst ExerciseEditor <|
                                Exercise.update exerciseConfig exerciseMsg m

                        x ->
                            ( x, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , exerciseCmd
            )

        CreateTarget target ->
            ( model
            , Cmd.map StoreMsg <| Store.createTarget target
            )

        DeleteTarget targetId ->
            ( model
            , Cmd.map StoreMsg <| Store.deleteTarget targetId
            )

        UpdateTarget target ->
            ( model
            , Cmd.map StoreMsg <| Store.updateTarget target
            )

        CreatePosition position ->
            ( model
            , Cmd.map StoreMsg <| Store.createPosition position
            )

        DeletePosition positionId ->
            ( model
            , Cmd.map StoreMsg <| Store.deletePosition positionId
            )

        UpdatePosition position ->
            ( model
            , Cmd.map StoreMsg <| Store.updatePosition position
            )

        ErrorAcked ->
            ( { model | httpError = Nothing }
            , Cmd.none
            )

        UpdateExercise exercise targets ->
            let
                redirect =
                    navigateToRoute model.navKey model.initialUrl (Router.Exercise exercise.id)

                updateCmd =
                    Cmd.map StoreMsg <| Store.updateExercise exercise
            in
            ( model, Cmd.batch [ redirect, updateCmd ] )

        CreateExercise _ _ ->
            -- TODO backend command
            ( model, Cmd.none )


targetConfig : Target.Config Msg
targetConfig =
    { createTarget = CreateTarget
    , deleteTarget = DeleteTarget
    , updateTarget = UpdateTarget
    }


positionConfig : Position.Config Msg
positionConfig =
    { createPosition = CreatePosition
    , deletePosition = DeletePosition
    , updatePosition = UpdatePosition
    }


exerciseConfig : Exercise.Config Msg
exerciseConfig =
    { updateExercise = UpdateExercise
    , createExercise = CreateExercise
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
            , E.width E.fill
            ]
            content
        ]


navigationLeft : Route -> Element Msg
navigationLeft currentRoute =
    E.column [ E.height E.fill, E.width (E.px 180) ]
        [ menuItem currentRoute Router.Home "Domů"
        , menuItem currentRoute Router.Exercises "Cviky"
        , menuItem currentRoute Router.Targets "Cílové partie"
        , menuItem currentRoute Router.Positions "Pozice"
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
