module Page.Routine exposing
    ( Config
    , DraggableItemId
    , Duration
    , ItemInRoutine
    , ItemPayload
    , Model
    , Msg
    , ValidationError(..)
    , editor
    , emptyEditor
    , initCopyEditor
    , initEditor
    , subscriptions
    , tableView
    , update
    , validationErrorToString
    , view
    )

import Browser.Dom as Dom
import Calendar
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
        , Inspiration
        , InspirationIdTag
        , ItemPayload(..)
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
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Id exposing (IdDict, IdSet)
import List.Extra as List
import Page.Exercise as Exercise
import Page.Routine.LessonPlanner as LessonPlanner exposing (LessonPlanner)
import Router
import Set.Any
import Task
import Time exposing (Posix)
import Time.Extra as Time


type alias Model =
    { routineRoute : Router.RoutineEditorRoute
    , topic : String
    , routineItems : List ItemInRoutine
    , dnd : DnDList.Model
    , tagFilter : IdSet TagIdTag
    , positionFilter : IdSet PositionIdTag
    , hasUnsavedChanges : Bool
    , showInspiration : Bool
    , inspirationOffset : Int
    , isScrolling : Bool
    }


type alias ItemInRoutine =
    { draggableItemId : DraggableItemId
    , itemPayload : ItemPayload
    , duration : Duration
    }


type ItemPayload
    = ItemPayloadExercise Exercise
    | ItemPayloadComment String
    | ItemPayloadEditedComment String String


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
    , noop : msg
    }


initEditor : IdDict ExerciseIdTag Exercise -> Routine -> Model
initEditor exercises routine =
    let
        routineItems =
            routine.items
                |> List.filterMap
                    (\item ->
                        Maybe.map (Tuple.pair item.duration) <|
                            case item.itemPayload of
                                IExerciseId eid ->
                                    Maybe.map ItemPayloadExercise <| Dict.Any.get eid exercises

                                IComment comment ->
                                    Just (ItemPayloadComment comment)
                    )
                |> List.indexedMap
                    (\index ( duration, payload ) ->
                        { draggableItemId = index
                        , itemPayload = payload
                        , duration = Duration duration
                        }
                    )
    in
    { routineRoute = Router.EditRoutine routine.id
    , routineItems = routineItems
    , topic = routine.topic
    , dnd = dndSystem.model
    , tagFilter = Id.emptySet
    , positionFilter = Id.emptySet
    , hasUnsavedChanges = False
    , showInspiration = False
    , inspirationOffset = 0
    , isScrolling = False
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
    , routineItems = []
    , topic = ""
    , dnd = dndSystem.model
    , tagFilter = Id.emptySet
    , positionFilter = Id.emptySet
    , hasUnsavedChanges = False
    , showInspiration = False
    , inspirationOffset = 0
    , isScrolling = True
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    dndSystem.subscriptions model.dnd


type Msg
    = AddExercise ExerciseId
    | AddComment
    | RemoveFromRoutine ItemInRoutine
    | ToggleTagId TagId
    | TogglePositionId PositionId
    | ChangeDuration DraggableItemId String
    | CommentStartEditing DraggableItemId
    | CommentUpdateText DraggableItemId String
    | CommentSaveEdits DraggableItemId
    | CommentDelete DraggableItemId
    | ChangeTopic String
    | ClearTagAndPositionFilters
    | SaveRoutine
    | ThrowAwayChanges
    | PrevInspiration
    | NextInspiration
    | ToggleInspiration
    | DnD DnDList.Msg
    | StartAutoScroll Float
    | SetViewPort Float Float Float Float


update : Config msg -> IdDict ExerciseIdTag Exercise -> Msg -> Model -> ( Model, Cmd msg )
update config exercises msg model =
    case msg of
        AddExercise exerciseId ->
            ( markUnsaved
                { model
                    | routineItems =
                        (Dict.Any.get exerciseId exercises
                            |> Maybe.map addExercise
                            |> Maybe.withDefault identity
                        )
                            model.routineItems
                }
            , Cmd.none
            )

        AddComment ->
            ( markUnsaved
                { model | routineItems = addComment model.routineItems }
            , Dom.focus newCommentInputId
                |> Task.attempt (always config.noop)
            )

        RemoveFromRoutine itemInRoutine ->
            ( markUnsaved
                { model
                    | routineItems =
                        List.filter (\item -> item /= itemInRoutine)
                            model.routineItems
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
            ( case parseDuration durationString of
                Nothing ->
                    model

                Just newDuration ->
                    markUnsaved
                        { model
                            | routineItems =
                                List.updateIf
                                    (\itemInRoutine -> itemInRoutine.draggableItemId == draggableItemId)
                                    (\itemInRoutine -> { itemInRoutine | duration = newDuration })
                                    model.routineItems
                        }
            , Cmd.none
            )

        CommentStartEditing draggableItemId ->
            ( markUnsaved
                { model
                    | routineItems =
                        List.updateIf
                            (\itemInRoutine -> itemInRoutine.draggableItemId == draggableItemId)
                            (\itemInRoutine ->
                                case itemInRoutine.itemPayload of
                                    ItemPayloadComment origComment ->
                                        { itemInRoutine | itemPayload = ItemPayloadEditedComment origComment origComment }

                                    _ ->
                                        itemInRoutine
                            )
                            model.routineItems
                }
            , Cmd.none
            )

        CommentSaveEdits draggableItemId ->
            ( markUnsaved
                { model
                    | routineItems =
                        List.updateIf
                            (\itemInRoutine -> itemInRoutine.draggableItemId == draggableItemId)
                            (\itemInRoutine ->
                                case itemInRoutine.itemPayload of
                                    ItemPayloadEditedComment _ newComment ->
                                        { itemInRoutine | itemPayload = ItemPayloadComment newComment }

                                    _ ->
                                        itemInRoutine
                            )
                            model.routineItems
                }
            , Cmd.none
            )

        CommentUpdateText draggableItemId newComment ->
            ( markUnsaved
                { model
                    | routineItems =
                        List.updateIf
                            (\itemInRoutine -> itemInRoutine.draggableItemId == draggableItemId)
                            (\itemInRoutine ->
                                case itemInRoutine.itemPayload of
                                    ItemPayloadEditedComment orig _ ->
                                        { itemInRoutine | itemPayload = ItemPayloadEditedComment orig newComment }

                                    _ ->
                                        itemInRoutine
                            )
                            model.routineItems
                }
            , Cmd.none
            )

        CommentDelete draggableItemId ->
            ( markUnsaved
                { model
                    | routineItems =
                        List.filter
                            (\itemInRoutine -> itemInRoutine.draggableItemId /= draggableItemId)
                            model.routineItems
                }
            , Cmd.none
            )

        ClearTagAndPositionFilters ->
            ( { model
                | tagFilter = Id.emptySet
                , positionFilter = Id.emptySet
              }
            , Cmd.none
            )

        DnD dndMsg ->
            let
                ( dnd, routineItems ) =
                    dndSystem.update dndMsg model.dnd model.routineItems

                ( isScrolling, maybeStartScrolling ) =
                    case dndSystem.info dnd of
                        Just { currentPosition, dragElement } ->
                            if currentPosition.y < 10 then
                                ( True
                                , if model.isScrolling then
                                    Cmd.none

                                  else
                                    {- Start scrolling UP when dragged element reaches the top of the viewport -}
                                    Command.perform <| StartAutoScroll -20
                                )

                            else if currentPosition.y + 10 > dragElement.viewport.height then
                                ( True
                                , if model.isScrolling then
                                    Cmd.none

                                  else
                                    {- Start scrolling DOWN when dragged element reaches the bottom of the viewport -}
                                    Command.perform <| StartAutoScroll 20
                                )

                            else
                                ( False, Cmd.none )

                        Nothing ->
                            ( False, Cmd.none )
            in
            ( markUnsaved
                { model
                    | dnd = dnd
                    , routineItems = routineItems
                    , isScrolling = isScrolling
                }
            , Cmd.map config.msg <|
                Cmd.batch
                    [ maybeStartScrolling
                    , dndSystem.commands model.dnd
                    ]
            )

        StartAutoScroll delta ->
            ( { model | isScrolling = True }
            , Task.perform
                (\{ scene, viewport } ->
                    config.msg <|
                        SetViewPort delta (scene.height - viewport.height) viewport.x viewport.y
                )
                Dom.getViewport
            )

        SetViewPort delta maxViewPortY viewPortX viewPortY ->
            if model.isScrolling then
                let
                    newViewPortY =
                        clamp 0 maxViewPortY <| viewPortY + delta
                in
                if newViewPortY == viewPortY then
                    ( { model | isScrolling = False }, Cmd.none )

                else
                    ( model
                    , Task.perform
                        (\_ -> config.msg <| SetViewPort delta maxViewPortY viewPortX newViewPortY)
                        (Dom.setViewport viewPortX viewPortY)
                    )

            else
                ( model, Cmd.none )

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

        ThrowAwayChanges ->
            ( model
            , Command.perform config.throwAwayChanges
            )

        PrevInspiration ->
            ( { model | inspirationOffset = model.inspirationOffset - 1 }
            , Cmd.none
            )

        NextInspiration ->
            ( { model | inspirationOffset = model.inspirationOffset + 1 }
            , Cmd.none
            )

        ToggleInspiration ->
            ( { model | showInspiration = not model.showInspiration }
            , Cmd.none
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


addExercise : Exercise -> List ItemInRoutine -> List ItemInRoutine
addExercise exercise list =
    List.indexedMap
        (\idx item -> { item | draggableItemId = idx })
        (list
            ++ [ { draggableItemId = 0
                 , itemPayload = ItemPayloadExercise exercise
                 , duration = Duration 3
                 }
               ]
        )


addComment : List ItemInRoutine -> List ItemInRoutine
addComment list =
    List.indexedMap
        (\idx item -> { item | draggableItemId = idx })
        (list
            ++ [ { draggableItemId = 0
                 , itemPayload = ItemPayloadEditedComment "" ""
                 , duration = Duration 0
                 }
               ]
        )


exercisesDurationMinutes : List ItemInRoutine -> Int
exercisesDurationMinutes =
    List.map (.duration >> durationToInt) >> List.sum


routineDurationMinutes : Routine -> Int
routineDurationMinutes routine =
    routine.items |> List.map .duration |> List.sum


view :
    Config msg
    -> IdDict ExerciseIdTag Exercise
    -> IdDict LessonIdTag Lesson
    -> Routine
    -> Maybe RoutineId
    -> Maybe RoutineId
    -> LessonPlanner
    -> Element msg
view config exercises lessons routine mPrevRoutineId mNextRoutineId lessonPlanner =
    E.column []
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , Common.heading1 routine.topic
        , E.column
            [ E.spacing 10 ]
            [ E.text <| "Celková délka: " ++ String.fromInt (routineDurationMinutes routine) ++ " minut."
            , E.row [ E.spacing 5, E.paddingXY 0 5 ]
                [ editRoutineButton routine
                , E.link Common.blueButton
                    { url = Router.href <| Router.RoutineEditor <| Router.CopyRoutine routine.id
                    , label = E.text "Kopírovat"
                    }
                , Input.button Common.coralButton
                    { onPress = Just (config.deleteRoutine routine.id)
                    , label = E.text "Odstranit"
                    }
                ]
            , E.row [ E.spacing 5 ]
                [ case mPrevRoutineId of
                    Just prevRoutineId ->
                        E.link Common.blueButton
                            { url = Router.href <| Router.Routine prevRoutineId
                            , label = E.text "Předchozí"
                            }

                    Nothing ->
                        E.none
                , case mNextRoutineId of
                    Just nextRoutineId ->
                        E.link Common.blueButton
                            { url = Router.href <| Router.Routine nextRoutineId
                            , label = E.text "Další"
                            }

                    Nothing ->
                        E.none
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
            , routine.items
                |> List.filterMap
                    (\item ->
                        Maybe.map (Tuple.pair item.duration) <|
                            case item.itemPayload of
                                IExerciseId eid ->
                                    Maybe.map ItemPayloadExercise <| Dict.Any.get eid exercises

                                IComment comment ->
                                    Just <| ItemPayloadComment comment
                    )
                |> splitInto30MinuteSegments
                |> (\segments ->
                        List.interweave
                            (List.map
                                (\segment ->
                                    E.column [ E.spacing 5 ] <|
                                        List.map routineItemView segment
                                )
                                segments
                            )
                            (List.range 1 (List.length segments - 1)
                                |> List.map
                                    (\x ->
                                        E.el
                                            [ E.centerX
                                            , E.width (E.px 680)
                                            , Background.color Color.lightGrey
                                            , Font.color Color.white
                                            ]
                                        <|
                                            E.el [ E.centerX ] <|
                                                E.text <|
                                                    String.fromInt (x * 30)
                                                        ++ " minut"
                                    )
                            )
                   )
                |> E.column [ E.paddingXY 0 5, E.spacing 5 ]
            ]
        ]


routineItemView : ( Int, ItemPayload ) -> Element msg
routineItemView ( duration, itemPayload ) =
    case itemPayload of
        ItemPayloadExercise exercise ->
            E.row [ Border.color Color.darkGrey, Border.width 1, E.spacing 5 ]
                [ Exercise.imagePreview exercise
                , E.link [ E.width (E.px 500) ]
                    { url = Router.href (Router.Exercise exercise.id)
                    , label = E.paragraph [] [ E.text exercise.name ]
                    }
                , E.el [ E.width (E.px 70) ] (E.text <| String.fromInt duration ++ " min")
                ]

        ItemPayloadComment comment ->
            commentItemView duration comment

        ItemPayloadEditedComment origComment _ ->
            commentItemView duration origComment


commentItemView : Int -> String -> Element msg
commentItemView duration comment =
    E.row [ Border.color Color.darkGrey, Border.width 1, E.spacing 5 ]
        [ commentBubble
        , E.el [ E.width (E.px 500) ] (E.text comment)
        , E.el [ E.width (E.px 70) ] (E.text <| String.fromInt duration ++ " min")
        ]


commentBubble : Element msg
commentBubble =
    E.el
        [ E.width (E.px Exercise.previewImageSize)
        , E.height (E.px Exercise.previewImageSize)
        , Font.color Color.black
        , Font.size 60
        ]
        (E.el [ E.centerX, E.centerY ] (E.text "🗨"))


splitInto30MinuteSegments : List ( Int, ItemPayload ) -> List (List ( Int, ItemPayload ))
splitInto30MinuteSegments itemsWithDuration =
    let
        f ( duration, exercise ) ( segments, runningDuration, nextThirty ) =
            case segments of
                -- Can't happen due to way the accumulator is initialized
                [] ->
                    ( [], 0, 0 )

                s0 :: other ->
                    if runningDuration + duration <= nextThirty then
                        ( (( duration, exercise ) :: s0) :: other
                        , runningDuration + duration
                        , nextThirty
                        )

                    else
                        ( [ ( duration, exercise ) ] :: s0 :: other
                        , runningDuration + duration
                        , nextThirty + 30
                        )
    in
    List.foldl f ( [ [] ], 0, 30 ) itemsWithDuration
        |> (\( segments, _, _ ) -> segments)
        |> List.map List.reverse
        |> List.reverse


lessonsUsingRoutine : IdDict LessonIdTag Lesson -> Routine -> List Lesson
lessonsUsingRoutine lessons routine =
    Dict.Any.filter (\_ lesson -> lesson.routineId == routine.id) lessons
        |> Dict.Any.values


tableView : Maybe Exercise -> IdDict RoutineIdTag Routine -> IdDict LessonIdTag Lesson -> Model -> Element Msg
tableView maybeExercise routines lessons model =
    let
        containsExercise : Routine -> Bool
        containsExercise routine =
            case maybeExercise of
                Just exercise ->
                    List.any (\routineItem -> routineItem.itemPayload == IExerciseId exercise.id)
                        routine.items

                Nothing ->
                    True

        createOrGoBackToEditedRoutine =
            if model.hasUnsavedChanges then
                returnToEditedRoutine model.routineRoute

            else
                createRoutineButton

        datesWhenUsedInLessons : Routine -> List Posix
        datesWhenUsedInLessons routine =
            lessonsUsingRoutine lessons routine
                |> List.map .datetime
                |> List.sortBy Time.posixToMillis
                |> List.reverse
    in
    E.column []
        [ Common.heading1 <|
            case maybeExercise of
                Just exercise ->
                    "Sestavy obsahující cvik \"" ++ exercise.name ++ "\""

                Nothing ->
                    "Sestavy"
        , createOrGoBackToEditedRoutine
        , case
            Dict.Any.values routines
                |> List.filter containsExercise
                |> List.sortBy .topic
          of
            [] ->
                E.text "Tento cvik dosud nebyl použit v žádné sestavě"

            nonEmptyRoutines ->
                let
                    cell =
                        E.el
                            [ Border.solid
                            , Border.width 1
                            , Border.color Color.lightGrey
                            , E.height E.fill
                            , E.padding 5
                            ]
                in
                E.table
                    [ Border.solid
                    , Border.width 1
                    , Border.color Color.lightGrey
                    ]
                    { data = nonEmptyRoutines
                    , columns =
                        [ { header = cell <| E.el [ E.centerY, E.centerX ] <| E.text "Název"
                          , width = E.shrink
                          , view =
                                \routine ->
                                    cell <|
                                        E.el [ E.alignLeft ] <|
                                            E.link Common.linkAttrs
                                                { url = Router.href (Router.Routine routine.id)
                                                , label = E.text routine.topic
                                                }
                          }
                        , { header = cell <| E.el [ E.centerY, E.centerX ] <| E.text "Naposledy použita"
                          , width = E.fill
                          , view =
                                \routine ->
                                    cell <|
                                        E.text <|
                                            case datesWhenUsedInLessons routine of
                                                [] ->
                                                    "Nikdy"

                                                a :: b :: c :: _ :: _ ->
                                                    [ a, b, c ]
                                                        |> List.map Time.formatDate
                                                        |> String.join ", "
                                                        |> (\txt -> txt ++ " ...")

                                                oneTwoOrThree ->
                                                    oneTwoOrThree
                                                        |> List.map Time.formatDate
                                                        |> String.join ", "
                          }
                        ]
                    }
        ]


editRoutineButton : Routine -> Element msg
editRoutineButton routine =
    E.link Common.blueButton
        { url = Router.href <| Router.RoutineEditor <| Router.EditRoutine routine.id
        , label = E.text "Upravit"
        }


createRoutineButton : Element msg
createRoutineButton =
    E.el [ E.paddingXY 0 5 ]
        (E.link Common.blueButton
            { url = Router.href (Router.RoutineEditor Router.NewRoutine)
            , label = E.text "Vytvořit sestavu"
            }
        )


returnToEditedRoutine : Router.RoutineEditorRoute -> Element msg
returnToEditedRoutine routineRoute =
    E.el [ E.paddingXY 0 5 ]
        (E.link Common.blueButton
            { url = Router.href (Router.RoutineEditor routineRoute)
            , label = E.text "Vrátit se k neuložené sestavě"
            }
        )


backToRoutineListLink : Element msg
backToRoutineListLink =
    E.link Common.linkAttrs
        { url = Router.href (Router.Routines Nothing)
        , label = E.text "« Zpět na seznam sestav"
        }


addCommentButton : Element Msg
addCommentButton =
    Input.button Common.blueButton
        { onPress = Just AddComment
        , label = E.text "Přidat komentář"
        }


saveButton : Element Msg
saveButton =
    Input.button Common.blueButton
        { onPress = Just SaveRoutine
        , label = E.text "Uložit"
        }


throwAwayEditsButton : Element Msg
throwAwayEditsButton =
    Input.button Common.coralButton
        { onPress = Just ThrowAwayChanges
        , label = E.text "Zahodit změny"
        }


editor :
    IdDict ExerciseIdTag Exercise
    -> IdDict TagIdTag Tag
    -> IdDict PositionIdTag Position
    -> IdDict RoutineIdTag Routine
    -> IdDict LessonIdTag Lesson
    -> IdDict InspirationIdTag Inspiration
    -> Posix
    -> Model
    -> Element Msg
editor exercises tags positions routines lessons inspirations today model =
    let
        pastExerciseUsages =
            getPastExerciseUsages today routines lessons

        colAttrs =
            [ E.alignTop
            , E.width <| E.fillPortion 3
            , E.height E.fill
            , Border.solid
            , Border.width 1
            ]

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
    E.column [ E.width E.fill ]
        [ backToRoutineListLink
        , E.row
            [ Border.solid
            , Border.width 1
            , E.width E.fill
            ]
            [ filtersColumn tags positions exercises model filteredExercises
            , E.column colAttrs
                [ E.el [ Font.bold, E.padding 5 ] (E.text "Dostupné cviky")
                , E.column
                    [ E.scrollbarY
                    , E.height E.fill
                    , E.paddingEach
                        { top = 0
                        , right = 15
                        , bottom = 0
                        , left = 0
                        }
                    ]
                  <|
                    List.map (availableExerciseView pastExerciseUsages) filteredExercises
                ]
            , E.column
                (E.inFront (ghostView model.dnd model.routineItems) :: E.spacing 5 :: colAttrs)
                (E.el [ Font.bold, E.padding 5 ] (E.text "Sestava")
                    :: Input.text
                        [ E.width (E.px 600)
                        , E.height (E.px 30)
                        , E.padding 4
                        ]
                        { onChange = ChangeTopic
                        , text = model.topic
                        , placeholder = Nothing
                        , label = Input.labelLeft [ E.padding 5 ] (E.text "Téma")
                        }
                    :: inspirationView inspirations today model.inspirationOffset model.showInspiration
                    :: List.indexedMap (draggableExercise model.dnd) model.routineItems
                    ++ [ E.el [ E.padding 5 ] addCommentButton
                       , E.el [ E.padding 5 ] <|
                            E.text <|
                                "Celková délka "
                                    ++ String.fromInt (exercisesDurationMinutes model.routineItems)
                                    ++ " min"
                       , if model.hasUnsavedChanges then
                            E.row [ E.padding 5, E.spacing 5 ]
                                [ saveButton, throwAwayEditsButton ]

                         else
                            E.none
                       ]
                )
            ]
        ]


inspirationView : IdDict InspirationIdTag Inspiration -> Posix -> Int -> Bool -> Element Msg
inspirationView inspirations today offset showingInspiration =
    if showingInspiration then
        let
            monthNum =
                Calendar.monthToInt <| Time.toMonth Time.utc today

            inspirationId =
                (monthNum + offset - 1)
                    |> modBy 12
                    |> (+) 1
                    |> Id.fromInt
        in
        Dict.Any.get inspirationId inspirations
            |> Maybe.map
                (\i ->
                    E.column [ E.paddingXY 5 0 ]
                        [ E.row [ E.spacing 5, E.width E.fill ]
                            [ Input.button navButtonAttrs
                                { onPress = Just PrevInspiration
                                , label = E.el [ E.centerY ] (E.text "«")
                                }
                            , E.el [ Font.bold, E.padding 5 ] <| E.text <| "Inspirace na " ++ Time.toCzechMonth (Time.monthFromNumber i.monthNumber)
                            , Input.button navButtonAttrs
                                { onPress = Just NextInspiration
                                , label = E.el [ E.centerY ] (E.text "»")
                                }
                            , Input.button (E.alignRight :: Common.blueButton)
                                { onPress = Just ToggleInspiration
                                , label = E.text "Skrýt Inspiraci"
                                }
                            ]
                        , E.paragraph []
                            [ Common.markdown i.description ]
                        ]
                )
            |> Maybe.withDefault E.none

    else
        E.column [ E.paddingXY 5 0 ]
            [ Input.button Common.blueButton
                { onPress = Just ToggleInspiration
                , label = E.text "Zobrazit inspiraci"
                }
            ]


navButtonAttrs : List (E.Attribute msg)
navButtonAttrs =
    [ E.padding 5
    , Border.color Color.darkGrey
    , Border.solid
    , Border.width 1
    , Border.rounded 5
    ]


filtersColumn :
    IdDict TagIdTag Tag
    -> IdDict PositionIdTag Position
    -> IdDict ExerciseIdTag Exercise
    -> Model
    -> List Exercise
    -> Element Msg
filtersColumn tags positions exercises model filteredExercises =
    E.column
        [ E.paddingXY 5 0
        , E.alignTop
        , E.width <| E.fillPortion 1
        , E.height E.fill
        , Border.solid
        , Border.width 1
        ]
        [ E.column [ E.alignTop ]
            [ if Set.Any.size model.tagFilter + Set.Any.size model.positionFilter > 0 then
                E.el [ E.padding 5 ]
                    (Input.button Common.blueButton
                        { onPress = Just ClearTagAndPositionFilters
                        , label = E.text "Zrušit filtry"
                        }
                    )

              else
                E.el [ Font.bold, E.padding 11 ] (E.text "Filtry")
            , Exercise.tagCheckboxes ToggleTagId 1000 tags model.tagFilter
            , positionCheckboxes positions model.positionFilter
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


availableExerciseView : IdDict ExerciseIdTag ExerciseUsages -> Exercise -> Element Msg
availableExerciseView pastExerciseUsages exercise =
    E.row
        [ Border.solid
        , Border.width 1
        , Border.color Color.lightGrey
        , E.width E.fill
        , E.spacing 5
        ]
        [ Exercise.imagePreview exercise
        , E.column
            [ E.width E.fill
            , E.alignTop
            , E.padding 5
            , E.spacing 5
            ]
            [ E.paragraph [ E.width E.fill, Font.bold ]
                [ E.link []
                    { url = Router.href (Router.Exercise exercise.id)
                    , label = E.text exercise.name
                    }
                ]
            , case exercise.sanskritName of
                Just sanskritName ->
                    E.paragraph [ E.width E.fill, Font.italic ]
                        [ E.link []
                            { url = Router.href (Router.Exercise exercise.id)
                            , label = E.text sanskritName
                            }
                        ]

                Nothing ->
                    E.none
            ]
        , E.el
            [ E.alignTop
            , Border.solid
            , Border.width 1
            , Border.color Color.midGrey
            , E.padding 5
            , E.alignRight
            , Font.color Color.midGrey
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
        , Input.button (E.alignRight :: E.alignTop :: navButtonAttrs)
            { onPress = Just (AddExercise exercise.id)
            , label = E.el [ E.centerY ] (E.text "»")
            }
        ]


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
                            List.filterMap
                                (\routineItem ->
                                    case routineItem.itemPayload of
                                        IExerciseId eid ->
                                            Just ( eid, routine.topic )

                                        IComment _ ->
                                            Nothing
                                )
                                routine.items

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


draggableExercise : DnDList.Model -> Int -> ItemInRoutine -> Element Msg
draggableExercise dnd index itemInRoutine =
    let
        exId =
            String.fromInt itemInRoutine.draggableItemId
    in
    E.row [ E.width E.fill, E.paddingXY 5 0, E.spacing 5 ]
        [ case itemInRoutine.itemPayload of
            ItemPayloadExercise _ ->
                Input.button navButtonAttrs
                    { onPress = Just (RemoveFromRoutine itemInRoutine)
                    , label = E.text "«"
                    }

            ItemPayloadComment _ ->
                E.el [ E.width (E.px 23) ] E.none

            ItemPayloadEditedComment _ _ ->
                E.el [ E.width (E.px 23) ] E.none
        , Input.text
            [ E.width (E.px 50)
            , E.height (E.px 30)
            , E.padding 4
            , E.htmlAttribute (Attr.type_ "number")
            ]
            { onChange = ChangeDuration itemInRoutine.draggableItemId
            , text =
                case itemInRoutine.duration of
                    Duration minutes ->
                        String.fromInt minutes

                    Empty ->
                        ""
            , placeholder = Nothing
            , label = Input.labelHidden "duration"
            }
        , draggableExerciseElement itemInRoutine <|
            case dndSystem.info dnd of
                Just { dragIndex } ->
                    if dragIndex /= index then
                        List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dropEvents index exId

                    else
                        [ Font.color (E.rgb255 156 156 156) ]

                Nothing ->
                    List.map E.htmlAttribute <| Attr.id exId :: dndSystem.dragEvents index exId
        ]


draggableExerciseElement : ItemInRoutine -> List (E.Attribute Msg) -> Element Msg
draggableExerciseElement iir attrs =
    case iir.itemPayload of
        ItemPayloadExercise exercise ->
            E.el attrs <| E.text exercise.name

        ItemPayloadComment comment ->
            E.el attrs <|
                E.row [ E.spacing 5 ]
                    [ E.text comment
                    , Common.iconButton (CommentStartEditing iir.draggableItemId) "🖉"
                    , Common.iconButton (CommentDelete iir.draggableItemId) "🗑"
                    ]

        ItemPayloadEditedComment _ editedValue ->
            E.row [ E.spacing 5 ]
                [ Input.text
                    [ E.width (E.px 300)
                    , E.height E.fill
                    , E.padding 5
                    , E.htmlAttribute (Attr.id newCommentInputId)
                    ]
                    { onChange = CommentUpdateText iir.draggableItemId
                    , text = editedValue
                    , placeholder = Nothing
                    , label = Input.labelHidden "Komentář"
                    }

                -- TODO save comment on Enter
                , Common.iconButton (CommentSaveEdits iir.draggableItemId) "💾"
                ]


ghostView : DnDList.Model -> List ItemInRoutine -> Element Msg
ghostView dnd routineItems =
    dndSystem.info dnd
        |> Maybe.andThen (\{ dragIndex } -> List.getAt dragIndex routineItems)
        |> Maybe.map
            (\itemInRoutine ->
                draggableExerciseElement itemInRoutine <|
                    List.map E.htmlAttribute <|
                        dndSystem.ghostStyles dnd
            )
        |> Maybe.withDefault E.none


dndConfig : DnDList.Config ItemInRoutine
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Vertical
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System ItemInRoutine Msg
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
                case model.routineItems of
                    [] ->
                        Err EmptyListOfExercises

                    nonemptyListOfItems ->
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
                                , items =
                                    List.map
                                        (\itemInRoutine ->
                                            { itemPayload =
                                                case itemInRoutine.itemPayload of
                                                    ItemPayloadExercise exercise ->
                                                        IExerciseId exercise.id

                                                    ItemPayloadComment comment ->
                                                        IComment comment

                                                    ItemPayloadEditedComment origComment _ ->
                                                        IComment origComment
                                            , duration = durationToInt itemInRoutine.duration
                                            }
                                        )
                                        nonemptyListOfItems
                                }
            )


backToList : Element msg
backToList =
    E.link Common.linkAttrs
        { url = Router.href (Router.Routines Nothing)
        , label = E.text "« Zpět na seznam sestav"
        }


newCommentInputId : String
newCommentInputId =
    "position-input"
