module Main exposing (Modal, Model, Msg, PageModel, main)

import Browser exposing (Document, UrlRequest)
import Browser.Events
import Browser.Navigation as Nav exposing (Key)
import Common
import Dict.Any
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , Inspiration
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
import Html.Events
import Http.Extra as Ht2
import Id
import Json.Decode as Decode exposing (Decoder)
import List.Extra as List
import Modal
import Page.Exercise as Exercise
import Page.Image as Image
import Page.Inspiration as Inspiration
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

    -- Unlike other page models, which are scoped to individual pages
    -- we keep Routine editor model here, so it's preserved on route changes.
    , routineModel : Routine.Model
    }


type Modal
    = HttpError Ht2.Error
    | ExerciseValidationError Exercise.ValidationError
    | RoutineValidationError Routine.ValidationError
    | YesNoModal (Element Never) Msg Msg
    | YesCloseModal String Msg
    | LessonPlannerValidationError String


initPageModel : Model -> Model
initPageModel model =
    case model.route of
        Router.Home ->
            { model | pageModel = HomeModel }

        Router.Positions ->
            { model | pageModel = PositionModel Position.init }

        Router.Tags ->
            { model | pageModel = TagModel Tag.init }

        Router.Lessons ->
            { model | pageModel = LessonModel }

        Router.Exercises ->
            { model | pageModel = ExerciseList }

        Router.Exercise exerciseId ->
            { model | pageModel = ExerciseModel exerciseId }

        Router.ExerciseEditor maybeExerciseId ->
            { model
                | pageModel =
                    maybeExerciseId
                        |> Maybe.andThen (\exerciseId -> Dict.Any.get exerciseId model.store.exercises)
                        |> Maybe.map (\exercise -> Exercise.initEditor exercise)
                        |> Maybe.withDefault Exercise.emptyEditor
                        |> ExerciseEditor
            }

        Router.Routines mayExerciseId ->
            { model | pageModel = RoutineList mayExerciseId }

        Router.Routine routineId ->
            { model | pageModel = RoutineModel routineId (LessonPlanner.init model.today routineId) }

        Router.RoutineEditor newRoutineRoute ->
            let
                ( newRoutineModel, newModal ) =
                    if model.routineModel.hasUnsavedChanges then
                        if model.routineModel.routineRoute == newRoutineRoute then
                            -- Navigating to previously edited thing
                            ( model.routineModel, Nothing )

                        else
                            -- Navigating to something new
                            ( model.routineModel
                            , Just <|
                                YesNoModal
                                    (E.column []
                                        [ E.text "Máš neuložené změny sestavy. Opravdu je chceš zahodit?"
                                        , E.text "Ano = zahodit změny"
                                        , E.text "Ne  = návrat k neuloženým změnám."
                                        ]
                                    )
                                    (SetRoutineEditor <| initRoutineEditor newRoutineRoute model.store)
                                    (SetRoutineEditor model.routineModel)
                            )

                    else
                        ( initRoutineEditor newRoutineRoute model.store, Nothing )
            in
            { model
                | pageModel = RoutineEditor
                , routineModel = newRoutineModel
                , modal = newModal
            }

        Router.Images ->
            { model | pageModel = ImagesModel }

        Router.Inspirations ->
            { model | pageModel = InspirationModel (Inspiration.init model.today) }

        Router.NotFound what ->
            { model | pageModel = NotFoundModel what }


initRoutineEditor : Router.RoutineEditorRoute -> Store -> Routine.Model
initRoutineEditor routineRoute store =
    case routineRoute of
        Router.NewRoutine ->
            Routine.emptyEditor

        Router.CopyRoutine routineId ->
            Dict.Any.get routineId store.routines
                |> Maybe.map (\routine -> Routine.initCopyEditor store.exercises routine)
                |> Maybe.withDefault Routine.emptyEditor

        Router.EditRoutine routineId ->
            Dict.Any.get routineId store.routines
                |> Maybe.map (\routine -> Routine.initEditor store.exercises routine)
                |> Maybe.withDefault Routine.emptyEditor


type PageModel
    = HomeModel
    | TagModel Tag.Model
    | LessonModel
    | InspirationModel Inspiration.Model
    | PositionModel Position.Model
      -- Exercise
    | ExerciseList
    | ExerciseModel ExerciseId
    | ExerciseEditor Exercise.Model
      -- Routine
    | RoutineList (Maybe ExerciseId)
    | RoutineModel RoutineId LessonPlanner
      -- Routine model is kept in the main model
    | RoutineEditor
    | ImagesModel
    | NotFoundModel String


type Msg
    = UrlRequest UrlRequest
    | UrlChange Url
    | GotTime Posix
    | StoreMsg Store.Msg
    | SetRoute Route
    | SetRoutineEditor Routine.Model
      -- Inspiration
    | InspirationMsg Inspiration.Msg
    | UpdateInspiration Inspiration
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
    | ExerciseMsg Exercise.Msg
    | UpdateExercise Exercise
    | CreateExercise Exercise
    | DeleteExercise ExerciseId
    | GotExerciseValidationError Exercise.ValidationError
      -- Routine
    | RoutineMsg Routine.Msg
    | CreateRoutine Routine
    | UpdateRoutine Routine
    | DeleteRoutine RoutineId
    | GotRoutineValidationError Routine.ValidationError
    | ThrowAwayRoutineEditorChanges
      -- Lesson Planner
    | LessonPlannerMsg LessonPlanner.Msg
    | GotLessonPlannerValidationError String
    | CreateLesson Lesson
    | DeleteLesson LessonId
    | DeleteImage String
    | CloseModal
    | CloseModalAndPerform Msg
    | ConfirmAction String Msg
    | NoOp


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( initPageModel
        { navKey = key
        , store = Store.init
        , route = Router.parseUrl url
        , initialUrl = url
        , pageModel = HomeModel
        , modal = Nothing
        , today = Time.millisToPosix 0
        , routineModel = Routine.emptyEditor
        }
    , Cmd.batch
        [ Cmd.map StoreMsg <|
            Cmd.batch
                [ Store.getTags
                , Store.getExercises
                , Store.getPositions
                , Store.getRoutines
                , Store.getLessons
                , Store.getInspirations
                , Store.verifyImages
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
                { closeMsg = CloseModal
                , title = "Něco se podělalo ☹"
                , bodyText = Ht2.errorToString httpError
                }

        ExerciseValidationError validationError ->
            Modal.viewError
                { closeMsg = CloseModal
                , title = "Toto cvičení není možno uložit"
                , bodyText = Exercise.validationErrorToString validationError
                }

        RoutineValidationError validationError ->
            Modal.viewError
                { closeMsg = CloseModal
                , title = "Tuto sestavu není možno uložit"
                , bodyText = Routine.validationErrorToString validationError
                }

        YesCloseModal message onConfirm ->
            Modal.confirmAction
                { cancelMsg = CloseModal
                , confirmMsg = CloseModalAndPerform onConfirm
                , title = "Upozornění"
                , body = E.text message
                }

        YesNoModal body onYes onNo ->
            Modal.confirmAction
                { cancelMsg = CloseModalAndPerform onNo
                , confirmMsg = CloseModalAndPerform onYes
                , title = "Upozornění"
                , body = body
                }

        LessonPlannerValidationError validationError ->
            Modal.viewError
                { closeMsg = CloseModal
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
                Exercise.listWithDetail exerciseConfig
                    model.store.positions
                    model.store.exercises
                    model.store.tags
                    Nothing

            ExerciseModel exerciseId ->
                case Dict.Any.get exerciseId model.store.exercises of
                    Just exercise ->
                        Exercise.listWithDetail exerciseConfig
                            model.store.positions
                            model.store.exercises
                            model.store.tags
                            (Just exercise)

                    Nothing ->
                        E.text <| "Cvičení s ID " ++ Id.toString exerciseId ++ " neexistuje"

            ExerciseEditor emodel ->
                E.map ExerciseMsg <|
                    Exercise.viewEditor
                        model.store.positions
                        model.store.tags
                        model.store.images
                        emodel

            TagModel tmodel ->
                E.map TagMsg <| Tag.view model.store.tags tmodel

            LessonModel ->
                Lesson.view lessonConfig
                    model.store.lessons
                    model.store.routines

            InspirationModel imodel ->
                E.map InspirationMsg <| Inspiration.view model.store.inspirations imodel

            PositionModel pmodel ->
                E.map PositionMsg <| Position.view model.store.positions pmodel

            RoutineList maybeExerciseId ->
                E.map RoutineMsg <|
                    Routine.tableView
                        (Maybe.andThen (\exerciseId -> Dict.Any.get exerciseId model.store.exercises) maybeExerciseId)
                        model.store.routines
                        model.store.lessons
                        model.routineModel

            RoutineModel routineId lessonPlanner ->
                case Dict.Any.get routineId model.store.routines of
                    Just routine ->
                        let
                            -- For prev/next navigation
                            ( maybeBeforeRoutineId, maybeAfterRoutineId ) =
                                case
                                    Dict.Any.values model.store.routines
                                        |> List.sortBy .topic
                                        |> List.map .id
                                        |> List.splitWhen (\rId -> rId == routineId)
                                of
                                    Just ( routinesBefore, routinesRest ) ->
                                        ( List.last routinesBefore
                                        , List.getAt 1 routinesRest
                                        )

                                    Nothing ->
                                        ( Nothing, Nothing )
                        in
                        Routine.view routineConfig
                            model.store.exercises
                            model.store.lessons
                            routine
                            maybeBeforeRoutineId
                            maybeAfterRoutineId
                            lessonPlanner

                    Nothing ->
                        E.text <| "Sestava s ID " ++ Id.toString routineId ++ " neexistuje"

            RoutineEditor ->
                E.map RoutineMsg <|
                    Routine.editor
                        model.store.exercises
                        model.store.tags
                        model.store.positions
                        model.store.routines
                        model.store.lessons
                        model.store.inspirations
                        model.today
                        model.routineModel

            ImagesModel ->
                Image.view imagesConfig model.store.images

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

                maybeReinitializePageModel =
                    case storeMsg of
                        Store.RoutinesFetched _ ->
                            initPageModel

                        Store.ExercisesFetched _ ->
                            initPageModel

                        _ ->
                            identity
            in
            ( { model
                | store = newStore
                , modal = Maybe.map HttpError maybeError
                , routineModel =
                    if Store.routineSavedSuccess storeMsg then
                        Routine.emptyEditor

                    else
                        model.routineModel
              }
                |> maybeReinitializePageModel
            , Store.redirect storeMsg
                |> Maybe.map goToRoute
                |> Maybe.withDefault Cmd.none
            )

        UrlChange url ->
            ( initPageModel { model | route = Router.parseUrl url }
            , Cmd.none
            )

        SetRoute newRoute ->
            ( model
            , if newRoute == Router.Images then
                Cmd.batch
                    [ Cmd.map StoreMsg Store.verifyImages
                    , goToRoute newRoute
                    ]

              else
                goToRoute newRoute
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

        InspirationMsg inspirationMsg ->
            let
                ( newPageModel, inspirationCmd ) =
                    case model.pageModel of
                        InspirationModel m ->
                            Tuple.mapFirst InspirationModel <|
                                Inspiration.update inspirationConfig inspirationMsg m

                        other ->
                            ( other, Cmd.none )
            in
            ( { model | pageModel = newPageModel }
            , inspirationCmd
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
                ( newRoutineModel, routineCmd ) =
                    Routine.update routineConfig model.store.exercises routineMsg model.routineModel
            in
            ( { model | routineModel = newRoutineModel }
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

        UpdateInspiration inspiration ->
            ( model
            , Cmd.map StoreMsg <| Store.updateInspiration inspiration
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

        CloseModal ->
            ( { model | modal = Nothing }
            , Cmd.none
            )

        CloseModalAndPerform anotherMsg ->
            update anotherMsg { model | modal = Nothing }

        CreateExercise exercise ->
            ( model
            , Cmd.map StoreMsg <| Store.createExercise exercise
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
            , Cmd.map StoreMsg <| Store.createRoutine routine
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
                , goToRoute (Router.Routines Nothing)
                ]
            )

        GotRoutineValidationError validationError ->
            ( { model | modal = Just (RoutineValidationError validationError) }
            , Cmd.none
            )

        ThrowAwayRoutineEditorChanges ->
            ( { model
                | routineModel =
                    case model.route of
                        Router.RoutineEditor maybeRoutineId ->
                            initRoutineEditor maybeRoutineId model.store

                        _ ->
                            Routine.emptyEditor
              }
            , Cmd.none
            )

        ConfirmAction message onConfirm ->
            ( { model | modal = Just (YesCloseModal message onConfirm) }
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

        GotLessonPlannerValidationError validationError ->
            ( { model | modal = Just (LessonPlannerValidationError validationError) }
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

        SetRoutineEditor routineModel ->
            ( { model | routineModel = routineModel }
            , Cmd.none
            )

        DeleteImage imageFileName ->
            ( model
            , Cmd.map StoreMsg <| Store.deleteImage imageFileName
            )

        NoOp ->
            ( model, Cmd.none )


inspirationConfig : Inspiration.Config Msg
inspirationConfig =
    { updateInspiration = UpdateInspiration
    }


tagConfig : Tag.Config Msg
tagConfig =
    { createTag = CreateTag
    , deleteTag = ConfirmAction "Opravdu chceš odstranit tento tag?" << DeleteTag
    , updateTag = UpdateTag
    , noop = NoOp
    }


positionConfig : Position.Config Msg
positionConfig =
    { createPosition = CreatePosition
    , deletePosition = ConfirmAction "Opravdu chceš odstranit tuto pozici?" << DeletePosition
    , updatePosition = UpdatePosition
    , noop = NoOp
    }


exerciseConfig : Exercise.Config Msg
exerciseConfig =
    { updateExercise = UpdateExercise
    , createExercise = CreateExercise
    , deleteExercise = ConfirmAction "Opravdu chceš odstranit tento cvik?" << DeleteExercise
    , validationError = GotExerciseValidationError
    }


routineConfig : Routine.Config Msg
routineConfig =
    { msg = RoutineMsg
    , createRoutine = CreateRoutine
    , updateRoutine = UpdateRoutine
    , deleteRoutine = ConfirmAction "Opravdu chceš odstranit tuto sestavu?" << DeleteRoutine
    , validationError = GotRoutineValidationError
    , lessonPlannerMsg = LessonPlannerMsg
    , throwAwayChanges = ConfirmAction "Opravdu chceš zahodit neuložené změny?" ThrowAwayRoutineEditorChanges
    , noop = NoOp
    }


lessonPlannerConfig : LessonPlanner.Config Msg
lessonPlannerConfig =
    { validationError = GotLessonPlannerValidationError
    , createLesson = CreateLesson
    }


lessonConfig : Lesson.Config Msg
lessonConfig =
    { deleteLesson = ConfirmAction "Opravdu chceš odstranit tuto lekci?" << DeleteLesson
    }


imagesConfig : Image.Config Msg
imagesConfig =
    { deleteImage = ConfirmAction "Opravdu chceš odstranit tento obrázek?" << DeleteImage
    }


navigateToRoute : Key -> Url -> Route -> Cmd msg
navigateToRoute key initialUrl route =
    Nav.pushUrl key <|
        Url.toString { initialUrl | fragment = Just <| Router.toHash route }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.pageModel of
            RoutineEditor ->
                Sub.map RoutineMsg <| Routine.subscriptions model.routineModel

            _ ->
                Sub.none
        , case model.modal of
            Just modal ->
                modalEscapeOrEnterSubscriptions modal

            Nothing ->
                Sub.none
        ]


modalEscapeOrEnterSubscriptions : Modal -> Sub Msg
modalEscapeOrEnterSubscriptions modal =
    case modal of
        HttpError _ ->
            Browser.Events.onKeyUp (fireOnEscape CloseModal)

        ExerciseValidationError _ ->
            Browser.Events.onKeyUp (fireOnEscape CloseModal)

        RoutineValidationError _ ->
            Browser.Events.onKeyUp (fireOnEscape CloseModal)

        LessonPlannerValidationError _ ->
            Browser.Events.onKeyUp (fireOnEscape CloseModal)

        YesCloseModal _ onConfirm ->
            Sub.batch
                [ Browser.Events.onKeyUp (fireOnEscape CloseModal)
                , Browser.Events.onKeyUp (fireOnEnter onConfirm)
                ]

        YesNoModal _ onYes onNo ->
            Sub.batch
                [ Browser.Events.onKeyUp (fireOnEscape onNo)
                , Browser.Events.onKeyUp (fireOnEnter onYes)
                ]


fireOnEscape : Msg -> Decoder Msg
fireOnEscape =
    fireOnKeyCode 27


fireOnEnter : Msg -> Decoder Msg
fireOnEnter =
    fireOnKeyCode 13


fireOnKeyCode : Int -> Msg -> Decoder Msg
fireOnKeyCode theKeyCode message =
    Html.Events.keyCode
        |> Decode.andThen
            (\keyCode ->
                if keyCode == theKeyCode then
                    Decode.succeed message

                else
                    Decode.fail "wrong key"
            )


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
        , menuItem currentRoute (Router.Routines Nothing) "Sestavy"
        , menuItem currentRoute Router.Lessons "Lekce"
        , menuItem currentRoute Router.Inspirations "Inspirace"
        , menuItem currentRoute Router.Images "Obrázky"
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
