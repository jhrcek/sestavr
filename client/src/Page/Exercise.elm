module Page.Exercise exposing
    ( Model
    , emptyEditor
    , initEditor
    , view
    , viewEditor
    , viewList
    )

import Dict.Any
import Domain exposing (Exercise, ExerciseId, ExerciseIdTag, TargetIdTag)
import Element as E exposing (Element)
import Element.Font as Font
import Id exposing (IdDict, IdSet)
import Markdown


type alias Model =
    { exerciseId : Maybe ExerciseId
    , name : String
    , sanskritName : String
    , description : String
    , targetAreas : IdSet TargetIdTag
    }


initEditor : Exercise -> Model
initEditor exercise =
    { exerciseId = Just exercise.id
    , name = exercise.name
    , sanskritName = Maybe.withDefault "" exercise.sanskritName
    , description = exercise.description
    , targetAreas = Id.emptySet
    }


emptyEditor : Model
emptyEditor =
    { exerciseId = Nothing
    , name = ""
    , sanskritName = ""
    , description = ""
    , targetAreas = Id.emptySet
    }


viewEditor : Model -> Element msg
viewEditor model =
    E.column []
        [ E.text <| Debug.toString model
        , E.link []
            { url =
                "/#exercise"
                    ++ (case model.exerciseId of
                            Just exerciseId ->
                                "/" ++ Id.toString exerciseId

                            Nothing ->
                                ""
                       )
            , label = E.text "Zrušit"
            }
        ]


viewList : IdDict ExerciseIdTag Exercise -> Element msg
viewList exercises =
    Dict.Any.values exercises
        |> List.map exerciseLink
        |> E.column []


exerciseLink : Exercise -> Element msg
exerciseLink exercise =
    E.row []
        [ E.link
            [ E.mouseOver [ E.moveRight 2 ]
            , Font.color lightBlue
            ]
            --TODO type-safe generation of exercise links
            { url = "/#exercise/" ++ Id.toString exercise.id
            , label =
                E.text
                    (exercise.name
                        ++ Maybe.withDefault ""
                            (Maybe.map (\s -> " (" ++ s ++ ")")
                                exercise.sanskritName
                            )
                    )
            }
        ]


lightBlue : E.Color
lightBlue =
    E.rgb255 18 147 216


view : Exercise -> Element msg
view e =
    E.column []
        [ E.el [ E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 } ] backToList
        , E.el [ Font.size 28, Font.bold ] (E.text e.name)
        , E.el [ Font.size 20, Font.bold, Font.italic ]
            (E.text <| Maybe.withDefault "sanskrit name?" e.sanskritName)
        , E.html <| Markdown.toHtml [] e.description
        , E.link []
            { url = "/#exercise/" ++ Id.toString e.id ++ "/edit"
            , label = E.text "Upravit"
            }
        ]


backToList : Element msg
backToList =
    E.link
        [ E.mouseOver [ E.moveLeft 2 ], Font.color lightBlue ]
        { url = "/#exercise"
        , label = E.text "« Zpět na seznam cviků"
        }
