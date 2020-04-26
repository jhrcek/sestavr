module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Common
import Dict.Any
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , Lesson
        , LessonId
        , Position
        , PositionId
        , Routine
        , RoutineId
        , Tag
        , TagId
        )
import Element as E exposing (Element)
import Element.Background as Background
import Element.Events as Event
import Element.Font as Font
import Http.Extra as Ht2
import Id
import Modal
import Page.Exercise as Exercise
import Page.Lesson as Lesson
import Page.Position as Position
import Page.Routine as Routine
import Page.Routine.LessonPlanner as LessonPlanner exposing (LessonPlanner)
import Page.Tag as Tag
import Router exposing (Route)
import Store exposing (Store)
import Task
import Time exposing (Posix)
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
    , modal : Maybe Modal
    , today : Posix
    }


type Modal
    = HttpError Ht2.Error
    | ExerciseValidationError Exercise.ValidationError
    | RoutineValidationError Routine.ValidationError
    | ConfirmDeletionModal String Msg
    | LessonPlannerValitaionError String


initPage : Route -> Store -> Posix -> PageModel
initPage route store today =
    case route of
        Router.Home ->
            HomeModel

        Router.Positions ->
            PositionModel Position.init

        Router.Tags ->
            TagModel Tag.init

        Router.Lessons ->
            LessonModel

        Router.Exercises ->
            ExerciseList

        Router.Exercise exerciseId ->
            ExerciseModel exerciseId

        Router.ExerciseEditor maybeExerciseId ->
            maybeExerciseId
                |> Maybe.andThen (\exerciseId -> Dict.Any.get exerciseId store.exercises)
                |> Maybe.map (\exercise -> Exercise.initEditor exercise)
                |> Maybe.withDefault Exercise.emptyEditor
                |> ExerciseEditor

        Router.Routines ->
            RoutineList

        Router.Routine routineId ->
            RoutineModel routineId (LessonPlanner.init today routineId)

        Router.RoutineEditor maybeRoutineId ->
            maybeRoutineId
                |> Maybe.andThen (\routineId -> Dict.Any.get routineId store.routines)
                |> Maybe.map (\routine -> Routine.initEditor store.exercises routine)
                |> Maybe.withDefault Routine.emptyEditor
                |> RoutineEditor

        Router.NotFound what ->
            NotFoundModel what


type PageModel
    = HomeModel
    | TagModel Tag.Model
    | LessonModel
    | PositionModel Position.Model
      -- Exercise
    | ExerciseList
    | ExerciseModel ExerciseId
    | ExerciseEditor Exercise.Model
      -- Routine
    | RoutineList
    | RoutineModel RoutineId LessonPlanner
    | RoutineEditor Routine.Model
    | NotFoundModel String


type Msg
    = UrlRequest UrlRequest
    | UrlChange Url
    | GotTime Posix
    | StoreMsg Store.Msg
    | SetRoute Route
    | ExerciseMsg Exercise.Msg
      -- Tag
    | TagMsg Tag.Msg
    | CreateTag Tag
    | DeleteTag TagId
    | UpdateTag Tag
      -- Position
    | PositionMsg Position.Msg
    | CreatePosition Position
    | DeletePosition PositionId
    | UpdatePosition Position
      -- Exercise
    | UpdateExercise Exercise
    | CreateExercise Exercise
    | DeleteExercise ExerciseId
    | GotExerciseValidationError Exercise.ValidationError
      -- Routine
    | RoutineMsg Routine.Msg
    | CreateRoutine Routine
    | CopyRoutine Routine
    | UpdateRoutine Routine
    | DeleteRoutine RoutineId
    | GotRoutineValidationError Routine.ValidationError
      -- Lesson Planner
    | LessonPlannerMsg LessonPlanner.Msg
    | GotLessonPlannerValitaionError String
    | CreateLesson Lesson
    | DeleteLesson LessonId
    | ErrorAcked
    | ConfirmDeletion String Msg
    | NoOp


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            Router.parseUrl url

        initTime =
            Time.millisToPosix 0
    in
    ( { navKey = key
      , store = Store.init
      , route = route
      , initialUrl = url
      , pageModel = initPage route Store.init initTime
      , modal = Nothing
      , today = initTime
      }
    , Cmd.batch
        [ Cmd.map StoreMsg <|
            Cmd.batch
                [ Store.getTags
                , Store.getExercises
                , Store.getPositions
                , Store.getRoutines
                , Store.getLessons
                ]
        , Task.perform GotTime Time.now
        ]
    )


view : Model -> Document Msg
view model =
    { title = "sestavr"
    , body =
        [ let
            attrsWithModal =
                case model.modal of
                    Just modal ->
                        [ E.inFront <| viewModal modal ]

                    Nothing ->
                        []
          in
          E.layout attrsWithModal (viewBody model)
        ]
    }


viewModal : Modal -> Element Msg
viewModal modal =
    case modal of
        HttpError httpError ->
            Modal.viewError
                { closeMsg = ErrorAcked
                , title = "Něco se podělalo ☹"
                , bodyText = Ht2.errorToString httpError
                }

        ExerciseValidationError validationError ->
            Modal.viewError
                { closeMsg = ErrorAcked
                , title = "Toto cvičení není možno uložit"
                , bodyText = Exercise.validationErrorToString validationError
                }

        RoutineValidationError validationError ->
            Modal.viewError
                { closeMsg = ErrorAcked
                , title = "Tuto sestavu není možno uložit"
                , bodyText = Routine.validationErrorToString validationError
                }

        ConfirmDeletionModal message onConfirm ->
            Modal.confirmDeletion
                { cancelMsg = ErrorAcked
                , confirmMsg = onConfirm
                , title = "Opravdu?"
                , bodyText = message
                }

        LessonPlannerValitaionError validationError ->
            Modal.viewError
                { closeMsg = ErrorAcked
                , title = "Tuto lekci není možné uložit"
                , bodyText = validationError
                }


viewBody : Model -> Element Msg
viewBody model =
    viewLayout model.route <|
        case model.pageModel of
            HomeModel ->
                E.column []
                    [ Common.heading1 "Úvodní stránka"
                    , E.text "Je něco užitečného, cy by mělo smysl zobrazit na úvodní stránce?"
                    ]

            ExerciseList ->
                Exercise.listView model.store.exercises

            ExerciseModel exerciseId ->
                case Dict.Any.get exerciseId model.store.exercises of
                    Just exercise ->
                        Exercise.view exerciseConfig
                            model.store.positions
                            model.store.tags
                            exercise

                    Nothing ->
                        E.text <| "Cvičení s ID " ++ Id.toString exerciseId ++ " neexistuje"

            ExerciseEditor emodel ->
                E.map ExerciseMsg <|
                    Exercise.viewEditor
                        model.store.positions
                        model.store.tags
                        emodel

            TagModel tmodel ->
                E.map TagMsg <| Tag.view model.store.tags tmodel

            LessonModel ->
                Lesson.view lessonConfig
                    model.store.lessons
                    model.store.routines

            PositionModel pmodel ->
                E.map PositionMsg <| Position.view model.store.positions pmodel

            RoutineList ->
                E.map RoutineMsg <| Routine.listView model.store.routines

            RoutineModel routineId lessonPlanner ->
                case Dict.Any.get routineId model.store.routines of
                    Just routine ->
                        Routine.view routineConfig
                            model.store.exercises
                            model.store.lessons
                            routine
                            lessonPlanner

                    Nothing ->
                        E.text <| "Sestava s ID " ++ Id.toString routineId ++ " neexistuje"

            RoutineEditor rmodel ->
                E.map RoutineMsg <|
                    Routine.editor
                        model.store.exercises
                        model.store.tags
                        model.store.positions
                        model.store.routines
                        model.store.lessons
                        model.today
                        rmodel

            NotFoundModel what ->
                E.text <| "Tady nic není : " ++ what


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        goToRoute =
            navigateToRoute model.navKey model.initialUrl
    in
    case msg of
        StoreMsg storeMsg ->
            let
                ( newStore, maybeError ) =
                    Store.update storeMsg model.store
            in
            ( { model
                | store = newStore
                , modal = Maybe.map HttpError maybeError
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
                , pageModel = initPage newRoute model.store model.today
              }
            , Cmd.none
            )

        SetRoute route ->
            ( model
            , goToRoute route
            )

        UrlRequest urlRequest ->
            ( model
            , case urlRequest of
                Browser.Internal url ->
                    Nav.pushUrl model.navKey (Url.toString url)

                Browser.External url ->
                    Nav.load url
            )

        TagMsg tagMsg ->
            let
                ( newPageModel, tagCmd ) =
                    case model.pageModel of
                        TagModel m ->
                            Tuple.mapFirst TagModel <|
                                Tag.update tagConfig tagMsg m

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , tagCmd
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

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , exerciseCmd
            )

        RoutineMsg routineMsg ->
            let
                ( newPageModel, routineCmd ) =
                    case model.pageModel of
                        RoutineEditor m ->
                            Tuple.mapFirst RoutineEditor <|
                                Routine.update routineConfig model.store.exercises routineMsg m

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , routineCmd
            )

        LessonPlannerMsg lpMsg ->
            let
                ( newPageModel, lpCmd ) =
                    case model.pageModel of
                        RoutineModel rid lessonPlanner ->
                            Tuple.mapFirst (RoutineModel rid) <|
                                LessonPlanner.update lessonPlannerConfig lpMsg lessonPlanner

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , lpCmd
            )

        CreateTag tag ->
            ( model
            , Cmd.map StoreMsg <| Store.createTag tag
            )

        DeleteTag tagId ->
            ( model
            , Cmd.map StoreMsg <| Store.deleteTag tagId
            )

        UpdateTag tag ->
            ( model
            , Cmd.map StoreMsg <| Store.updateTag tag
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
            ( { model | modal = Nothing }
            , Cmd.none
            )

        CreateExercise exercise ->
            ( model
            , Cmd.batch
                [ Cmd.map StoreMsg <| Store.createExercise exercise
                , goToRoute Router.Exercises
                ]
            )

        UpdateExercise exercise ->
            ( model
            , Cmd.batch
                [ Cmd.map StoreMsg <| Store.updateExercise exercise
                , goToRoute <| Router.Exercise exercise.id
                ]
            )

        DeleteExercise exerciseId ->
            ( model
            , Cmd.batch
                [ Cmd.map StoreMsg <| Store.deleteExercise exerciseId
                , goToRoute Router.Exercises
                ]
            )

        GotExerciseValidationError validationError ->
            ( { model | modal = Just (ExerciseValidationError validationError) }
            , Cmd.none
            )

        CreateRoutine routine ->
            ( model
            , Cmd.batch
                [ Cmd.map StoreMsg <| Store.createRoutine routine
                , goToRoute Router.Routines
                ]
            )

        CopyRoutine routine ->
            ( { model | pageModel = RoutineEditor <| Routine.initCopyEditor model.store.exercises routine }
            , Cmd.none
            )

        UpdateRoutine routine ->
            ( model
            , Cmd.batch
                [ Cmd.map StoreMsg <| Store.updateRoutine routine
                , goToRoute <| Router.Routine routine.id
                ]
            )

        DeleteRoutine routineId ->
            ( model
            , Cmd.batch
                [ Cmd.map StoreMsg <| Store.deleteRoutine routineId
                , goToRoute Router.Routines
                ]
            )

        GotRoutineValidationError validationError ->
            ( { model | modal = Just (RoutineValidationError validationError) }
            , Cmd.none
            )

        ConfirmDeletion message onConfirm ->
            ( { model | modal = Just (ConfirmDeletionModal message onConfirm) }
            , Cmd.none
            )

        GotTime posix ->
            ( { model
                | today = posix
                , pageModel =
                    case model.pageModel of
                        RoutineModel rid _ ->
                            RoutineModel rid (LessonPlanner.init posix rid)

                        other ->
                            other
              }
            , Cmd.none
            )

        GotLessonPlannerValitaionError validationError ->
            ( { model | modal = Just (LessonPlannerValitaionError validationError) }
            , Cmd.none
            )

        CreateLesson lesson ->
            ( model
            , Cmd.map StoreMsg <| Store.createLesson lesson
            )

        DeleteLesson lessonId ->
            ( model
            , Cmd.map StoreMsg <| Store.deleteLesson lessonId
            )

        NoOp ->
            ( model, Cmd.none )


tagConfig : Tag.Config Msg
tagConfig =
    { createTag = CreateTag
    , deleteTag = ConfirmDeletion "Opravdu chceš odstranit tento tag?" << DeleteTag
    , updateTag = UpdateTag
    , noop = NoOp
    }


positionConfig : Position.Config Msg
positionConfig =
    { createPosition = CreatePosition
    , deletePosition = ConfirmDeletion "Opravdu chceš odstranit tuto pozici?" << DeletePosition
    , updatePosition = UpdatePosition
    , noop = NoOp
    }


exerciseConfig : Exercise.Config Msg
exerciseConfig =
    { updateExercise = UpdateExercise
    , createExercise = CreateExercise
    , deleteExercise = ConfirmDeletion "Opravdu chceš odstranit tento cvik?" << DeleteExercise
    , validationError = GotExerciseValidationError
    }


routineConfig : Routine.Config Msg
routineConfig =
    { msg = RoutineMsg
    , createRoutine = CreateRoutine
    , copyRoutine = CopyRoutine
    , updateRoutine = UpdateRoutine
    , deleteRoutine = ConfirmDeletion "Opravdu chceš odstranit tuto sestavu?" << DeleteRoutine
    , validationError = GotRoutineValidationError
    , lessonPlannerMsg = LessonPlannerMsg
    }


lessonPlannerConfig : LessonPlanner.Config Msg
lessonPlannerConfig =
    { validationError = GotLessonPlannerValitaionError
    , createLesson = CreateLesson
    }


lessonConfig : Lesson.Config Msg
lessonConfig =
    { deleteLesson = ConfirmDeletion "Opravdu chceš odstranit tuto lekci?" << DeleteLesson
    }


navigateToRoute : Key -> Url -> Route -> Cmd msg
navigateToRoute key initialUrl route =
    Nav.pushUrl key <|
        Url.toString { initialUrl | fragment = Just <| Router.toHash route }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pageModel of
        RoutineEditor rmodel ->
            Sub.map RoutineMsg <| Routine.subscriptions rmodel

        _ ->
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
    E.column [ E.height E.fill ]
        [ menuItem currentRoute Router.Home "Domů"
        , menuItem currentRoute Router.Exercises "Cviky"
        , menuItem currentRoute Router.Tags "Tagy"
        , menuItem currentRoute Router.Positions "Pozice"
        , menuItem currentRoute Router.Routines "Sestavy"
        , menuItem currentRoute Router.Lessons "Lekce"
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
