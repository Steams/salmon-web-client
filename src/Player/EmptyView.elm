module Player.EmptyView exposing (view)

import Api exposing (login)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as HtmlAttribute
import Json.Encode as E
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles exposing (edges)



-- View


step heading instructions =
    Element.column
        [ width (px 350)
        , height (px 450)
        , Border.shadow { offset = ( -1, 1 ), size = 0, blur = 7, color = Styles.light_grey }
        , Background.color Styles.white
        , paddingEach { edges | bottom = 20 }
        ]
        [ el [ Background.color (rgb255 244 250 255), height (px 100), Font.bold, width fill, Font.color Styles.text_grey ] <| el [ centerY, centerX ] <| text heading
        , paragraph [ Font.center, paddingXY 25 30, Background.color Styles.white, height fill, width fill ] [ text instructions ]
        , Input.button
            [ height (px 50)
            , width (px 150)
            , Font.size 17
            , Font.center
            , centerX
            , Font.color Styles.white
            , Font.bold
            , Background.color (rgb255 79 59 121)
            ]
            { onPress = Nothing
            , label = text "Sign Up"
            }
        ]


steps =
    Element.el [ width fill, centerX ] <|
        Element.wrappedRow [ centerX, spacing 20, width shrink ]
            [ step "Download Media Server" "Download and install the salmon media server application"
            , step "Run Salmon Server" "Use your salmon log in credentials to run the salmon server and point it to your music library [exaaple command here]"
            , step "Refresh and Enjoy" "Refresh this page or Log in to any salmon client with your credentials to start streaming your music anywhere"
            ]


blurb =
    Element.column [ width (fillPortion 1 |> maximum 900), spacing 50, centerX ]
        [ column [ spacing 15, centerX ]
            [ el [ Font.color (rgb255 234 69 16), Font.bold, Font.size 40 ] <| text "Your Music Library Is Empty"
            ]
        , paragraph []
            [ text "Follow the instructions below to get started streaming your music blah blah blah extra text"
            ]
        ]


content =
    Element.column [ centerX, spacing 50, height fill ]
        [ blurb
        , steps
        ]

view model =
    Element.column [ width fill, height fill, Background.color (rgb255 240 244 247) , paddingEach {edges | top = 50}]
        [content]
