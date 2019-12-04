module Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font



-- Colors


blue =
    rgb255 5 100 245


primary_green =
    rgb255 0 209 178


dark_accent =
    rgb255 0 184 156


link_blue =
    rgb255 50 155 220


black =
    rgb255 0 0 0


white =
    rgb255 255 255 255


input_background =
    rgb255 244 248 248


text_grey =
    rgb255 125 125 125


text_black =
    rgb255 105 105 105


grey =
    rgb255 74 74 74


green =
    rgb255 120 145 155


light_grey =
    rgba255 0 0 0 0.2


red =
    rgb255 173 72 102


pink =
    rgb255 207 152 153



-- Fonts


font_small =
    Font.size 16


title value options =
    el
        ([ centerX
         , Font.size 35
         , Font.family [ Font.typeface "Roboto" ]
         , Font.bold
         , Font.color red
         ]
            ++ options
        )
    <|
        text value



-- Utils


edges =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }



-- Styles


link_item : List (Attribute msg)
link_item =
    [ Font.color link_blue
    , mouseOver [ Font.color black ]
    ]

nav_bar =
    [ width fill
    , Background.color primary_green
    , height (px 52)
    , font_small
    , paddingEach { edges | right = 10 }
    ]


nav_item =
    [ centerY, centerX ]


active_nav_item =
    [ height fill
    , px 75 |> width
    , Background.color dark_accent
    , Font.color white
    ]


inactive_nav_item =
    [ height fill
    , px 75 |> width
    , mouseOver [ Background.color dark_accent ]
    , Font.color white
    ]
