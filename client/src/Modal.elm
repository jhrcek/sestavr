module Modal exposing (Config, confirmAction, viewError)

import Color
import Common
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


confirmAction :
    { cancelMsg : msg
    , confirmMsg : msg
    , title : String
    , body : Element Never
    }
    -> Element msg
confirmAction { cancelMsg, confirmMsg, title, body } =
    E.el
        [ Background.color Color.dialogMask
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
            [ header cancelMsg title
            , E.el [ E.padding 20 ] (E.map never body)
            , E.row [ E.spacing 5, E.alignRight, E.padding 5 ]
                [ Input.button Common.blueButton
                    { onPress = Just confirmMsg
                    , label = E.text "Ano"
                    }
                , Input.button Common.blueButton
                    { onPress = Just cancelMsg
                    , label = E.text "Ne"
                    }
                ]
            ]
        )


viewError : Config msg -> Element msg
viewError { title, bodyText, closeMsg } =
    E.el
        [ Background.color Color.dialogMask
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
            , E.paragraph
                [ E.padding 20 ]
                [ E.text bodyText ]
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
