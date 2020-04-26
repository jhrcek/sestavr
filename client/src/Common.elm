module Common exposing
    ( buttonAttrs
    , heading1
    , iconButton
    , linkAttrs
    )

import Color
import Element as E exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


buttonAttrs : List (Attribute msg)
buttonAttrs =
    [ E.padding 5
    , Border.solid
    , Border.width 1
    , Border.rounded 4
    ]


linkAttrs : List (Attribute msg)
linkAttrs =
    [ E.mouseOver [ Font.color Color.darkBlue ]
    , Font.color Color.lightBlue
    ]


heading1 : String -> Element msg
heading1 text =
    E.el
        [ Font.size 28
        , Font.bold
        , E.paddingXY 0 10
        ]
        (E.text text)


iconButton : { onPress : Maybe msg, label : Element msg } -> Element msg
iconButton =
    Input.button
        [ Border.solid
        , Border.width 1
        , Border.rounded 4
        , E.padding 5
        , E.mouseOver [ Background.color Color.lightGrey ]
        ]
