module Page.Routine exposing
    ( Config
    , Model
    , Msg
    , ValidationError(..)
    , editor
    , emptyEditor
    , initCopyEditor
    , initEditor
    , listView
    , subscriptions
    , update
    , validationErrorToString
    , view
    )

import Color
import Command
import Common
import Dict.Any
import DnDList
import Domain
    exposing
        ( Exercise
        , ExerciseId
        , ExerciseIdTag
        , Lesson
        , LessonIdTag
        , Position
        , PositionId
        , PositionIdTag
        , Routine
        , RoutineId
        , RoutineIdTag
        , Tag
        , TagId
        , TagIdTag
        )
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Id exposing (IdDict, IdSet)
import List.Extra as List
import Page.Exercise as Exercise
import Page.Routine.LessonPlanner as LessonPlanner exposing (LessonPlanner)
import Router
import Set.Any
import Time exposing (Posix)
import Time.Extra as Time


type alias Model =
    { routineRoute : Router.RoutineEditorRoute
    , topic : String
    , routineExercises : List ExerciseInRoutine
    , dnd : DnDList.Model
    , tagFilter : IdSet TagIdTag
    , positionFilter : IdSet PositionIdTag
    , showingPopupFor : Maybe ExerciseId
    , hasUnsavedChanges : Bool
    }


type alias ExerciseInRoutine =
    { draggableItemId : DraggableItemId
    , exercise : Exercise
    , duration : Duration
    }


type alias DraggableItemId =
    Int


type alias Config msg =
    { msg : Msg -> msg
    , createRoutine : Routine -> msg
    , updateRoutine : Routine -> msg
    , deleteRoutine : RoutineId -> msg
    , validationError : ValidationError -> msg
    , lessonPlannerMsg : LessonPlanner.Msg -> msg
    , throwAwayChanges : msg
    }


initEditor : IdDict ExerciseIdTag Exercise -> Routine -> Model
initEditor exercises routine =
    let
        routineExercises =
            routine.exercises
                |> List.filterMap (\re -> Dict.Any.get re.exerciseId exercises |> Maybe.map (Tuple.pair re))
                |> List.indexedMap
                    (\index ( re, exercise ) ->
                        { draggableItemId = index
                        , exercise = exercise
                        , duration = Duration re.duration
                        }
                    )
    in
    { routineRoute = Router.EditRoutine routine.id
    , routineExercises = routineExercises
    , topic = routine.topic
    , dnd = dndSystem.model
    , tagFilter = Id.emptySet
    , positionFilter = Id.emptySet
    , showingPopupFor = Nothing
    , hasUnsavedChanges = False
    }


initCopyEditor : IdDict ExerciseIdTag Exercise -> Routine -> Model
initCopyEditor exercises routine =
    let
        model =
            initEditor exercises routine
    in
    { model
        | routineRoute = Router.CopyRoutine routine.id
        , topic = model.topic ++ " (kopie)"
    }


emptyEditor : Model
emptyEditor =
    { routineRoute = Router.NewRoutine
    , routineExercises = []
    , topic = ""
    , dnd = dndSystem.model
    , tagFilter = Id.emptySet
    , positionFilter = Id.emptySet
    , showingPopupFor = Nothing
    , hasUnsavedChanges = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd


type Msg
    = AddToRoutine ExerciseId
    | RemoveFromRoutine ExerciseInRoutine
    | ToggleTagId TagId
    | TogglePositionId PositionId
    | ChangeDuration DraggableItemId String
    | ChangeTopic String
    | ClearTags
    | ClearPositions
    | SaveRoutine
    | ShowExerciseDetailsPopup ExerciseId
    | HideExerciseDetailsPopup
    | ThrowAwayChanges
    | DnD DnDList.Msg


update : Config msg -> IdDict ExerciseIdTag Exercise -> Msg -> Model -> ( Model, Cmd msg )
update config exercises msg model =
    case msg of
        AddToRoutine exerciseId ->
            ( markUnsaved
                { model
                    | routineExercises =
                        (Dict.Any.get exerciseId exercises
                            |> Maybe.map addExercise
                            |> Maybe.withDefault identity
                        )
                            model.routineExercises
                }
            , Cmd.none
            )

        RemoveFromRoutine exerciseInRoutine ->
            ( markUnsaved
                { model
                    | routineExercises =
                        List.filter (\eir -> eir /= exerciseInRoutine)
                            model.routineExercises
                }
            , Cmd.none
            )

        ToggleTagId tagId ->
            ( { model | tagFilter = Set.Any.toggle tagId model.tagFilter }
            , Cmd.none
            )

        TogglePositionId positionId ->
            ( { model | positionFilter = Set.Any.toggle positionId model.positionFilter }
            , Cmd.none
            )

        ChangeDuration draggableItemId durationString ->
            ( markUnsaved
                { model
                    | routineExercises =
                        case parseDuration durationString of
                            Nothing ->
                                model.routineExercises

                            Just newDuration ->
                                List.updateIf
                                    (\exerciseInRoutine -> exerciseInRoutine.draggableItemId == draggableItemId)
                                    (\exerciseIntRoutine -> { exerciseIntRoutine | duration = newDuration })
                                    model.routineExercises
                }
            , Cmd.none
            )

        ClearTags ->
            ( { model | tagFilter = Id.emptySet }
            , Cmd.none
            )

        ClearPositions ->
            ( { model | positionFilter = Id.emptySet }
            , Cmd.none
            )

        DnD dndMsg ->
            let
                ( dnd, routineExercises ) =
                    dndSystem.update dndMsg model.dnd model.routineExercises
            in
            ( markUnsaved
                { model
                    | dnd = dnd
                    , routineExercises = routineExercises
                }
            , Cmd.map config.msg <| dndSystem.commands model.dnd
            )

        SaveRoutine ->
            let
                command =
                    case updateOrCreate config model of
                        Err validationError ->
                            config.validationError validationError

                        Ok saveOrUpdateCmd ->
                            saveOrUpdateCmd
            in
            ( model
            , Command.perform command
            )

        ChangeTopic newTopic ->
            ( markUnsaved { model | topic = newTopic }
            , Cmd.none
            )

        ShowExerciseDetailsPopup exerciseIndex ->
            ( { model | showingPopupFor = Just exerciseIndex }
            , Cmd.none
            )

        HideExerciseDetailsPopup ->
            ( { model | showingPopupFor = Nothing }
            , Cmd.none
            )

        ThrowAwayChanges ->
            ( model
            , Command.perform config.throwAwayChanges
            )


markUnsaved : Model -> Model
markUnsaved model =
    { model | hasUnsavedChanges = True }


{-| This is to allow numbers only input the value of which can be deleted (corresponding to 0)
-}
type Duration
    = Empty
    | Duration Int


parseDuration : String -> Maybe Duration
parseDuration str =
    if String.isEmpty str then
        Just Empty

    else
        Maybe.map (Duration << max 0) <| String.toInt str


durationToInt : Duration -> Int
durationToInt duration =
    case duration of
        Empty ->
            0

        Duration x ->
            x


addExercise : Exercise -> List ExerciseInRoutine -> List ExerciseInRoutine
addExercise exercise list =
    List.indexedMap
        (\idx eir -> { eir | draggableItemId = idx })
        (list ++ [ { draggableItemId = 0, exercise = exercise, duration = Duration 3 } ])


exercisesDurationMinutes : List ExerciseInRoutine -> Int
exercisesDurationMinutes =
    List.map (.duration >> durationToInt) >> List.sum


routineDurationMinutes : Routine -> Int
routineDurationMinutes routine =
    routine.exercises |> List.map .duration |> List.sum


view :
    Config msg
    -> IdDict ExerciseIdTag Exercise
    -> IdDict LessonIdTag Lesson
    -> Routine
    -> LessonPlanner
    -> Element msg
view config exercises lessons routine lessonPlanner =
    E.column []
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , Common.heading1 routine.topic
        , E.column
            [ E.spacing 5 ]
            [ E.text <| "Počet cviků: " ++ String.fromInt (List.length routine.exercises)
            , E.text <| "Celková délka: " ++ String.fromInt (routineDurationMinutes routine) ++ " minut."
            , E.column [ E.paddingXY 0 5 ] <|
                List.indexedMap
                    (\idx ( duration, exercise ) ->
                        E.text <|
                            String.fromInt (idx + 1)
                                ++ ". "
                                ++ exercise.name
                                ++ ", "
                                ++ String.fromInt duration
                                ++ " min"
                    )
                <|
                    List.filterMap
                        (\re ->
                            Maybe.map (Tuple.pair re.duration) <|
                                Dict.Any.get re.exerciseId exercises
                        )
                        routine.exercises
            ]
        , E.row [ E.spacing 5, E.paddingXY 0 5 ]
            [ editRoutineButton routine
            , E.link Common.buttonAttrs
                { url = Router.href <| Router.RoutineEditor <| Router.CopyRoutine routine.id
                , label = E.text "Kopírovat"
                }
            , Input.button Common.buttonAttrs
                { onPress = Just (config.deleteRoutine routine.id)
                , label = E.text "Odstranit"
                }
            ]
        , E.el [ E.paddingXY 0 5 ] <|
            case lessonsUsingRoutine lessons routine of
                [] ->
                    E.text "Tato sestava zatím nebyla použita v žádné lekci"

                ls ->
                    E.column []
                        (E.text "Tato sestava byla použita v lekcích"
                            :: List.map (\lesson -> E.text <| Time.formatDateTime lesson.datetime) ls
                        )
        , E.map config.lessonPlannerMsg <| LessonPlanner.view lessonPlanner
        ]


lessonsUsingRoutine : IdDict LessonIdTag Lesson -> Routine -> List Lesson
lessonsUsingRoutine lessons routine =
    Dict.Any.filter (\_ lesson -> lesson.routineId == routine.id) lessons
        |> Dict.Any.values


listView : IdDict RoutineIdTag Routine -> Model -> Element Msg
listView routines model =
    let
        createOrGoBackToEditedRoutine =
            if model.hasUnsavedChanges then
                returnToEditedRoutine model.routineRoute

            else
                createRoutineButton
    in
    Dict.Any.values routines
        |> List.sortBy .topic
        |> List.map routineLink
        |> (\routineLinks -> createOrGoBackToEditedRoutine :: routineLinks)
        |> (::) (Common.heading1 "Sestavy")
        |> E.column []


routineLink : Routine -> Element msg
routineLink routine =
    E.link Common.linkAttrs
        { url = Router.href (Router.Routine routine.id)
        , label = E.text routine.topic
        }


editRoutineButton : Routine -> Element msg
editRoutineButton routine =
    E.link Common.buttonAttrs
        { url = Router.href <| Router.RoutineEditor <| Router.EditRoutine routine.id
        , label = E.text "Upravit"
        }


createRoutineButton : Element msg
createRoutineButton =
    E.el [ E.paddingXY 0 5 ]
        (E.link Common.buttonAttrs
            { url = Router.href (Router.RoutineEditor Router.NewRoutine)
            , label = E.text "Vytvořit sestavu"
            }
        )


returnToEditedRoutine : Router.RoutineEditorRoute -> Element msg
returnToEditedRoutine routineRoute =
    E.el [ E.paddingXY 0 5 ]
        (E.link Common.buttonAttrs
            { url = Router.href (Router.RoutineEditor routineRoute)
            , label = E.text "Vrátit se k neuložené sestavě"
            }
        )


backToRoutineListLink : Element msg
backToRoutineListLink =
    E.link Common.linkAttrs
        { url = Router.href Router.Routines
        , label = E.text "«  Zpět na seznam sestav"
        }


saveButton : Element Msg
saveButton =
    Input.button Common.buttonAttrs
        { onPress = Just SaveRoutine
        , label = E.text "Uložit"
        }


throwAwayEditsButton : Element Msg
throwAwayEditsButton =
    Input.button Common.buttonAttrs
        { onPress = Just ThrowAwayChanges
        , label = E.text "Zahodit změny"
        }


editor :
    IdDict ExerciseIdTag Exercise
    -> IdDict TagIdTag Tag
    -> IdDict PositionIdTag Position
    -> IdDict RoutineIdTag Routine
    -> IdDict LessonIdTag Lesson
    -> Posix
    -> Model
    -> Element Msg
editor exercises tags positions routines lessons today model =
    let
        pastExerciseUsages =
            getPastExerciseUsages today routines lessons

        colAttrs maxWidth =
            [ E.alignTop
            , E.width <| E.maximum maxWidth <| E.minimum exerciseColumnMinWidth E.fill
            , E.height E.fill
            , Border.solid
            , Border.width 1
            ]

        exerciseColumnMaxWidth =
            600

        exerciseColumnMinWidth =
            500

        filteredExercises =
            Dict.Any.values exercises
                |> List.sortBy .name
                |> (if Set.Any.isEmpty model.tagFilter then
                        identity

                    else
                        -- Keep only exercises that have at least one tag selected in tagFilter
                        List.filter (\exercise -> setAny (\tagId -> List.member tagId exercise.tagIds) model.tagFilter)
                   )
                |> (if Set.Any.isEmpty model.positionFilter then
                        identity

                    else
                        List.filter (\exercise -> Set.Any.member exercise.positionId model.positionFilter)
                   )
    in
    E.column []
        [ backToRoutineListLink
        , E.row
            [ Border.solid
            , Border.width 1
            , E.width E.fill
            ]
            [ E.column
                [ E.paddingXY 5 0
                , E.alignTop
                , E.width (E.px 300)
                , E.height E.fill
                , Border.solid
                , Border.width 1
                ]
                [ E.column [ E.alignTop ]
                    [ Exercise.tagCheckboxes ToggleTagId 1000 tags model.tagFilter
                    , if Set.Any.isEmpty model.tagFilter then
                        E.none

                      else
                        Input.button Common.buttonAttrs
                            { onPress = Just ClearTags, label = E.text "Zrušit výběr" }
                    , positionCheckboxes positions model.positionFilter
                    , if Set.Any.isEmpty model.positionFilter then
                        E.none

                      else
                        Input.button Common.buttonAttrs
                            { onPress = Just ClearPositions, label = E.text "Zrušit výběr" }
                    , let
                        filteredCount =
                            List.length filteredExercises

                        totalCount =
                            Dict.Any.size exercises
                      in
                      if totalCount > filteredCount then
                        E.paragraph []
                            [ E.text <|
                                String.fromInt filteredCount
                                    ++ " / "
                                    ++ String.fromInt totalCount
                                    ++ " cviků odpovídá kriteriím"
                            ]

                      else
                        E.none
                    ]
                ]
            , E.column (E.spacing 5 :: colAttrs exerciseColumnMaxWidth)
                (E.el [ Font.bold, E.padding 5 ] (E.text "Dostupné cviky")
                    :: List.map
                        (\exercise ->
                            E.row [ E.paddingXY 5 0, E.spacing 5, E.width E.fill ]
                                [ E.el [ E.padding 5 ]
                                    (E.text <| elipsis 35 exercise.name)
                                , E.el
                                    [ Border.solid
                                    , Border.width 1
                                    , Border.color Color.darkGrey
                                    , E.padding 5
                                    , E.alignRight
                                    , Font.color Color.darkGrey
                                    , Event.onMouseEnter (ShowExerciseDetailsPopup exercise.id)
                                    , Event.onMouseLeave HideExerciseDetailsPopup
                                    , E.below <|
                                        case model.showingPopupFor of
                                            Just exIdWithPopup ->
                                                if exercise.id == exIdWithPopup then
                                                    exerciseUsagePopup exercise pastExerciseUsages

                                                else
                                                    E.none

                                            Nothing ->
                                                E.none
                                    ]
                                    (E.text <|
                                        case Dict.Any.get exercise.id pastExerciseUsages of
                                            Just usages ->
                                                let
                                                    mostRecentPastUsage =
                                                        List.maximumBy (.lesson >> .datetime >> Time.posixToMillis) usages
                                                in
                                                case mostRecentPastUsage of
                                                    Just lastUsage ->
                                                        Time.formatDate lastUsage.lesson.datetime

                                                    Nothing ->
                                                        "Nikdy"

                                            Nothing ->
                                                "Nikdy"
                                    )
                                , Input.button (E.alignRight :: Common.buttonAttrs)
                                    { onPress = Just (AddToRoutine exercise.id)
                                    , label = E.text "»"
                                    }
                                ]
                        )
                        filteredExercises
                )
            , E.column
                (E.inFront (ghostView model.dnd model.routineExercises) :: E.spacing 5 :: colAttrs exerciseColumnMaxWidth)
                (E.el [ Font.bold, E.padding 5 ] (E.text "Sestava")
                    :: Input.text
                        [ E.width (E.px 250)
                        , E.height (E.px 30)
                        , E.padding 4
                        ]
                        { onChange = ChangeTopic
                        , text = model.topic
                        , placeholder = Nothing
                        , label = Input.labelLeft [ E.padding 5 ] (E.text "Téma")
                        }
                    :: List.indexedMap (draggableExercise model.dnd) model.routineExercises
                    ++ [ E.el [ E.padding 5 ] <|
                            E.text <|
                                "Celková délka "
                                    ++ String.fromInt (exercisesDurationMinutes model.routineExercises)
                                    ++ " min"
                       , E.row [ E.padding 5, E.spacing 5 ]
                            [ saveButton ]
                       , if model.hasUnsavedChanges then
                            E.column [ E.padding 5, E.spacing 5 ]
                                [ E.text "Editor obsahuje neuložené změny"
                                , throwAwayEditsButton
                                ]

                         else
                            E.none
                       ]
                )
            ]
        ]


elipsis : Int -> String -> String
elipsis maxLength s =
    if String.length s > maxLength then
        String.left maxLength s ++ "…"

    else
        s


type alias ExerciseUsages =
    List
        { lesson : Lesson
        , routineTopic : String
        }


{-| Calculate a Dict, which for each exercise contains list of PAST lessons, where that exercise was used
-}
getPastExerciseUsages :
    Posix
    -> IdDict RoutineIdTag Routine
    -> IdDict LessonIdTag Lesson
    -> IdDict ExerciseIdTag ExerciseUsages
getPastExerciseUsages today routines lessons =
    Dict.Any.foldl
        (\_ lesson exUsagesAcc ->
            let
                lessonExercises : List ( ExerciseId, String )
                lessonExercises =
                    case Dict.Any.get lesson.routineId routines of
                        Just routine ->
                            List.map (\routineExercise -> ( routineExercise.exerciseId, routine.topic )) routine.exercises

                        Nothing ->
                            []
            in
            List.foldl
                (\( exerciseId, routineTopic ) ->
                    Dict.Any.update exerciseId
                        (\maybeUsages ->
                            let
                                usage =
                                    { lesson = lesson
                                    , routineTopic = routineTopic
                                    }
                            in
                            case maybeUsages of
                                Just usages ->
                                    Just (usage :: usages)

                                Nothing ->
                                    Just [ usage ]
                        )
                )
                exUsagesAcc
                lessonExercises
        )
        Id.emptyDict
    <|
        Dict.Any.filter (\_ lesson -> Time.posixToMillis lesson.datetime < Time.posixToMillis today)
            lessons


exerciseUsagePopup :
    Exercise
    -> IdDict ExerciseIdTag ExerciseUsages
    -> Element msg
exerciseUsagePopup exercise exerciseUsages =
    case Dict.Any.get exercise.id exerciseUsages of
        Nothing ->
            E.none

        Just [] ->
            E.none

        Just usages ->
            E.column
                [ Border.width 1
                , Border.solid
                , Border.color Color.black
                , Background.color Color.white
                , Font.color Color.black
                , E.padding 10
                ]
            <|
                E.text "Naposledy použito"
                    :: List.map
                        (\{ lesson, routineTopic } ->
                            E.text <| Time.formatDateTime lesson.datetime ++ " - " ++ routineTopic
                        )
                        (List.sortBy
                            (negate << Time.posixToMillis << .datetime << .lesson)
                            usages
                        )


positionCheckboxes : IdDict PositionIdTag Position -> IdSet PositionIdTag -> Element Msg
positionCheckboxes positions selectedPositions =
    let
        positionCheckbox position =
            Input.checkbox []
                { onChange = \_ -> TogglePositionId position.id
                , icon = Input.defaultCheckbox
                , checked = Set.Any.member position.id selectedPositions
                , label = Input.labelRight [] (E.text position.name)
                }
    in
    E.column []
        [ E.el [ E.padding 3, Font.bold ]
            (E.text "Pozice")
        , E.column [] <|
            List.map positionCheckbox <|
                Dict.Any.values positions
        ]


setAny : (a -> Bool) -> Set.Any.AnySet Int a -> Bool
setAny p set =
    Set.Any.foldl (\a acc -> p a || acc) False set


draggableExercise : DnDList.Model -> Int -> ExerciseInRoutine -> Element Msg
draggableExercise dndModel index exerciseInRoutine =
    let
        exId =
            String.fromInt exerciseInRoutine.draggableItemId
    in
    E.row [ E.width E.fill ]
        [ draggableExerciseElement exerciseInRoutine <|
            case dndSystem.info dndModel of
                Just { dragIndex } ->
                    if dragIndex /= index then
                        List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dropEvents index exId

                    else
                        [ Font.color (E.rgb255 156 156 156) ]

                Nothing ->
                    List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dragEvents index exId
        , Input.text
            [ E.alignRight
            , E.width (E.px 50)
            , E.height (E.px 30)
            , E.padding 4
            , E.htmlAttribute (Attr.type_ "number")
            ]
            { onChange = ChangeDuration exerciseInRoutine.draggableItemId
            , text =
                case exerciseInRoutine.duration of
                    Empty ->
                        ""

                    Duration minutes ->
                        String.fromInt minutes
            , placeholder = Nothing
            , label = Input.labelHidden "duration"
            }
        ]


draggableExerciseElement : ExerciseInRoutine -> List (E.Attribute Msg) -> Element Msg
draggableExerciseElement eir attrs =
    E.row (E.paddingXY 5 0 :: attrs)
        [ Input.button Common.buttonAttrs
            { onPress = Just (RemoveFromRoutine eir)
            , label = E.text "«"
            }
        , E.el [ E.padding 5 ] (E.text eir.exercise.name)
        ]


ghostView : DnDList.Model -> List ExerciseInRoutine -> Element Msg
ghostView dndModel routineExercises =
    dndSystem.info dndModel
        |> Maybe.andThen (\{ dragIndex } -> List.getAt dragIndex routineExercises)
        |> Maybe.map
            (\exerciseInRoutine ->
                draggableExerciseElement exerciseInRoutine <|
                    List.map E.htmlAttribute <|
                        dndSystem.ghostStyles dndModel
            )
        |> Maybe.withDefault E.none


dndConfig : DnDList.Config ExerciseInRoutine
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System ExerciseInRoutine Msg
dndSystem =
    DnDList.create dndConfig DnD


type ValidationError
    = EmptyListOfExercises
    | TopicEmpty


validationErrorToString : ValidationError -> String
validationErrorToString ve =
    case ve of
        EmptyListOfExercises ->
            "Seznam cvičení nesmí být prázdný"

        TopicEmpty ->
            "Musíš nastavit téma sestavy"


updateOrCreate : Config msg -> Model -> Result ValidationError msg
updateOrCreate config model =
    (if String.isEmpty model.topic then
        Err TopicEmpty

     else
        Ok model.topic
    )
        |> Result.andThen
            (\validTopic ->
                case model.routineExercises of
                    [] ->
                        Err EmptyListOfExercises

                    nonemptyListOfExercises ->
                        let
                            ( request, id ) =
                                case model.routineRoute of
                                    Router.NewRoutine ->
                                        ( config.createRoutine, Id.fromInt -1 )

                                    Router.CopyRoutine _ ->
                                        ( config.createRoutine, Id.fromInt -1 )

                                    Router.EditRoutine routineId ->
                                        ( config.updateRoutine, routineId )
                        in
                        Ok <|
                            request
                                { id = id
                                , topic = validTopic
                                , exercises =
                                    List.map
                                        (\exerciseInRoutine ->
                                            { exerciseId = exerciseInRoutine.exercise.id
                                            , duration = durationToInt exerciseInRoutine.duration
                                            }
                                        )
                                        nonemptyListOfExercises
                                }
            )


backToList : Element msg
backToList =
    E.link Common.linkAttrs
        { url = Router.href Router.Routines
        , label = E.text "« Zpět na seznam sestav"
        }
