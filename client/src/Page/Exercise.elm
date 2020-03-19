module Page.Exercise exposing
    ( --Config
      --, Model
      --, Msg
      --, init
      --, update
      view
    )

import Dict.Any
import Domain exposing (Exercise, ExerciseIdTag)
import Element as E exposing (Element)
import Element.Font as Font
import Id exposing (IdDict)
import Markdown


view : IdDict ExerciseIdTag Exercise -> Element msg
view exercises =
    Dict.Any.values exercises
        |> List.map exerciseView
        |> E.column []


exerciseView : Exercise -> Element msg
exerciseView e =
    E.column []
        [ E.el [ Font.size 28, Font.bold ] (E.text e.name)
        , E.el [ Font.size 20, Font.bold, Font.italic ]
            (E.text <| Maybe.withDefault "sanskrit name?" e.sanskritName)
        , E.html <| Markdown.toHtml [] e.description
        ]
