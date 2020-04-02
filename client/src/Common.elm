module Common exposing (buttonAttrs)

import Element as E exposing (Attribute)
import Element.Border as Border


buttonAttrs : List (Attribute msg)
buttonAttrs =
    [ E.padding 5
    , Border.solid
    , Border.width 1
    , Border.rounded 4
    ]
