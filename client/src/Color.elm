module Color exposing
    ( black
    , coral
    , darkBlue
    , darkGrey
    , dialogMask
    , lightBlue
    , lightGrey
    , midGrey
    , orange
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
    E.rgb255 114 159 207


darkBlue : Color
darkBlue =
    E.rgb255 17 95 135


lightGrey : Color
lightGrey =
    E.rgb255 192 192 192


midGrey : Color
midGrey =
    E.rgb255 128 128 128


darkGrey : Color
darkGrey =
    E.rgb255 80 80 80


orange : Color
orange =
    E.rgb255 255 102 0


coral : Color
coral =
    E.rgb255 248 131 121
