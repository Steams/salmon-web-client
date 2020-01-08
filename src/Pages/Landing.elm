module Pages.Landing exposing (Model, Msg(..), init, update, view)

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
import Styles



-- Model


type alias Model =
    ()


type Msg
    = NoOp


init : Session -> ( Model, Cmd Msg )
init session =
    ( (), Cmd.none )


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        _ ->
            ( model
            , Cmd.none
            )



-- View


showcase =
    Element.column [ width (fillPortion 1) ]
        [ Element.image
            [ centerY
            , centerX
            , width (px 960)
            , height (px 540)
            , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
            ]
            { src = "http://localhost:8080/web_assets/demo_img.png", description = "" }
        ]


blurb =
    Element.column [ width (fillPortion 1 |> maximum 900), spacing 50 , paddingXY 100 0]
        [ column [ spacing 15 ]
            [ el [ Font.color (rgb255 234 69 16), Font.bold , Font.size 18] <| text "TAKE YOUR MUSIC ANYWHERE"
            , el [ Font.size 40, Font.bold ] <| text "Stream with Salmon"
            ]
        , paragraph []
            [ text "Your favorite movies, TV, music, web shows, podcasts, and more, all streamed to your favorite screens."
            , text " WHAT IS STREMIO? Stremio is a one-stop hub for video content aggregation. Discover, organize and watch video from all kind of sources on any device that you own. Movies, TV shows, series, live television or web channels like YouTube and Twitch.tv - you can find all this on Stremio."
            , text " Bringing all of your home videos, music, and photos together into one place has never been easier. Your personal Emby Server automatically converts and streams your media on-the-fly to play on any device."
            ]
        , Element.row [ spacing 10 ]
            [ Input.button
                [ height (px 50)
                , width (px 150)
                , Font.size 17
                , Font.center
                , Font.color Styles.white
                , Font.bold
                , Background.color (rgb255 198 63 96)
                ]
                { onPress = Nothing
                , label = text "Get Started"
                }
            , Input.button
                [ height (px 50)
                , width (px 150)
                , Font.size 17
                , Font.center
                , Font.color Styles.white
                , Font.bold
                , Background.color (rgb255 79 59 121)
                ]
                { onPress = Nothing
                , label = text "Github"
                }
            ]
        ]


content =
    Element.column [ centerX, spacing 50 ]
        [ blurb
        , showcase
        ]


topbar =
    Element.row [ height (px 70), width fill ]
        [ Element.row [ centerX, paddingXY 100 0, width fill ]
            [ el [ alignLeft , Font.bold] <| text "Salmon"
            , Element.row [ alignRight, spacing 20 ]
                [ Input.button
                    [ height (px 50)
                    , width (px 150)
                    , Font.size 17
                    , Font.center
                    , Font.color Styles.white
                    , Font.bold
                    , Background.color (rgb255 198 63 96)
                    ]
                    { onPress = Nothing
                    , label = text "Login"
                    }
                , Input.button
                    [ height (px 50)
                    , width (px 150)
                    , Font.size 17
                    , Font.center
                    , Font.color Styles.white
                    , Font.bold
                    , Background.color (rgb255 79 59 121)
                    ]
                    { onPress = Nothing
                    , label = text "Download"
                    }
                ]
            ]
        ]


render model =
    Element.column [ width fill, height fill, spacing 50 ]
        [ topbar
        , content
        ]


view : Model -> TitleAndContent Msg
view model =
    { title = "Login"
    , content = render model
    }
