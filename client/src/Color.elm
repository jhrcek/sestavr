module Color exposing
    ( black
    , darkBlue
    , darkGrey
    , dialogMask
    , lightBlue
    , lightGrey
    , white
    )

import Element as E exposing (Color)


dialogMask : Color
dialogMask =
    E.rgba 0 0 0 0.3


black : Color
black =
    E.rgb255 0 0 0


white : Color
white =
    E.rgb255 255 255 255


lightBlue : Color
lightBlue =
    E.rgb255 18 147 216


darkBlue : Color
darkBlue =
    E.rgb255 17 95 135


lightGrey : Color
lightGrey =
    E.rgb255 192 192 192


darkGrey : Color
darkGrey =
    E.rgb255 128 128 128
