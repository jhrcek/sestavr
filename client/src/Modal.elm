module Modal exposing (Config, viewError)

import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias Config msg =
    { title : String
    , bodyText : String
    , closeMsg : msg
    }


viewError : Config msg -> Element msg
viewError { title, bodyText, closeMsg } =
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
            , Border.width 1
            ]
            [ header closeMsg title
            , body bodyText
            ]
        )


header : msg -> String -> Element msg
header closeMsg title =
    E.row
        [ E.width E.fill
        , Background.color (E.rgb255 150 150 150)
        ]
        [ E.el [ E.centerX, E.padding 5 ] (E.text title)
        , Input.button [ E.alignRight, E.padding 1, Font.size 20, E.padding 5 ]
            { onPress = Just closeMsg
            , label = E.text "×"
            }
        ]


body : String -> Element msg
body bodyText =
    E.paragraph
        [ E.padding 20 ]
        [ E.text bodyText ]


dialogMask : E.Color
dialogMask =
    E.rgba 0 0 0 0.3
