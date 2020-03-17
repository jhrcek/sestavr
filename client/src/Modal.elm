module Modal exposing (viewError)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http.Extra as Ht2


viewError : msg -> Ht2.Error -> Element msg
viewError closeMsg error =
    E.el
        [ Background.color dialogMask
        , E.width E.fill
        , E.height E.fill
        ]
        (E.column
            [ E.width (E.px 600)
            , E.centerX
            , E.centerY
            , Background.color (E.rgb255 255 255 255)
            , Border.solid
            , Border.width 2
            ]
            [ header closeMsg
            , body error
            ]
        )


header : msg -> Element msg
header closeMsg =
    E.row
        [ E.width E.fill
        , Background.color (E.rgb255 150 150 150)
        ]
        [ E.el [ E.centerX, E.padding 5 ] (E.text "Něco se podělalo ☹")
        , Input.button [ E.alignRight, E.padding 1, Font.size 20, E.padding 5 ]
            { onPress = Just closeMsg
            , label = E.text "×"
            }
        ]


body : Ht2.Error -> Element msg
body error =
    E.paragraph [ E.padding 20 ]
        [ E.text (Ht2.errorToString error) ]


dialogMask : E.Color
dialogMask =
    E.rgba 0 0 0 0.3
