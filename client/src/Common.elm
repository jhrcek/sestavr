module Common exposing
    ( blueButton
    , coralButton
    , heading1
    , iconButton
    , linkAttrs
    )

import Color
import Element as E exposing (Attribute, Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


blueButton : List (Attribute msg)
blueButton =
    coloredButtonAttrs Color.lightBlue


coralButton : List (Attribute msg)
coralButton =
    coloredButtonAttrs Color.coral


coloredButtonAttrs : Color -> List (Attribute msg)
coloredButtonAttrs bgColor =
    [ E.padding 5
    , Border.color Color.darkGrey
    , Border.solid
    , Border.width 1
    , Border.rounded 5
    , Border.shadow
        { offset = ( 4, 4 )
        , size = 3
        , blur = 10
        , color = E.rgb255 208 208 208
        }
    , Background.color bgColor
    , Font.color Color.white
    , E.mouseOver [ Background.color Color.white, Font.color Color.black ]
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


iconButton : msg -> String -> Element msg
iconButton onPress label =
    Input.button
        [ Border.solid
        , Border.width 1
        , Border.rounded 4
        , Border.color Color.darkGrey
        , E.padding 5
        , E.width (E.px 32)
        , E.height (E.px 32)
        , E.mouseOver [ Background.color Color.lightGrey ]
        ]
        { onPress = Just onPress
        , label = E.el [ E.centerY, E.centerX ] (E.text label)
        }
