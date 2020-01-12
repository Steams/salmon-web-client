module Pages.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Api exposing (login)
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
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
import Task



-- Model


type alias Model =
    { username : String
    , password : String
    , viewport : { width : Int, height : Int }
    }


get_screen_info =
    Task.perform WindowInfo Dom.getViewport


type Msg
    = NoOp
    | UsernameInput String
    | PasswordInput String
    | SubmitLogin
    | LoginResponse (WebData String)
    | Signup
    | Resize Int Int
    | WindowInfo Dom.Viewport


init : Session -> ( Model, Cmd Msg )
init session =
    ( { username = ""
      , password = ""
      , viewport = { width = 0, height = 0 }
      }
    , get_screen_info
    )


login username password =
    Api.login LoginResponse username password


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        WindowInfo info ->
            let
                _ =
                    Debug.log "init window" info
            in
            ( { model | viewport = { width = round info.viewport.width, height = round info.viewport.height } }
            , Cmd.none
            )

        Resize w h ->
            let
                _ =
                    Debug.log "init window" w
            in
            ( { model | viewport = { width = w, height = h } }
            , Cmd.none
            )

        UsernameInput val ->
            ( { model | username = val }
            , Cmd.none
            )

        PasswordInput val ->
            ( { model | password = val }
            , Cmd.none
            )

        SubmitLogin ->
            ( { model | password = "", username = "" }
            , login model.username model.password
            )

        Signup ->
            ( model
            , Nav.pushUrl session.navKey (Route.toUrl Route.Signup)
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View


button value handler =
    Input.button
        [ height (px 60)
        , width (px 300)
        , centerX
        , Border.rounded 100
        , Border.width 2
        , Border.color Styles.blue
        , Font.size 12
        , Font.center
        , Font.color Styles.blue
        , Font.bold
        ]
        { onPress = Just handler
        , label = text value
        }


input placeholder handler value =
    Input.text
        [ width (px 400)
        , height (px 65)
        , centerX
        , paddingXY 20 22
        , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = Styles.black } ]
        , Border.width 0
        , Border.rounded 0
        , Font.size 20
        , Font.color Styles.text_grey
        , Background.color Styles.input_background
        ]
        { onChange = handler
        , text = value
        , placeholder = Just (Input.placeholder [] <| el [ centerY, Font.color (rgb255 200 200 200) ] <| text placeholder)
        , label = Input.labelHidden ""
        }


title value =
    el
        [ centerX
        , Font.size 35
        , Font.family [ Font.typeface "Roboto" ]
        , Font.bold
        , Font.color Styles.green
        ]
    <|
        text value


signup_panel device =
    Element.el
        [ width (fillPortion 2)
        , height fill
        , Background.color Styles.green
        , Font.color Styles.white
        ]
    <|
        Element.column [ centerY, centerX, spacing 40, paddingXY 0 30 ]
            [ el [ Font.size 25, Font.bold, Font.color Styles.white ] <| text "First Time? Create an account!"
            , paragraph [ width (px 400), centerX ]
                [ text "Join Salmon now to stream all ur media anywhere you have a pblah a w rest of sentence theere pargarpah the quick borwn fox"
                ]
            , button "SIGN UP" Signup
            ]


login_panel username password device =
    Element.column [ width (fillPortion 3), spacing 60, centerY, paddingXY 0 30 ]
        [ el [ Font.size 25, centerX, Font.bold, Font.color Styles.green ] <| text "Log in to Salmon"
        , Element.column [ centerX, spacing 20 ]
            [ input "Username" UsernameInput username
            , input "Password" PasswordInput password
            ]
        , el [ centerX ] <| text "Forgot your password ?"
        , button "LOG IN" SubmitLogin
        ]


render model =
    let
        device =
            Element.classifyDevice model.viewport

        page =
            case device.class of
                Phone ->
                    Element.column [ width fill, height fill ]
                        [ login_panel model.username model.password Phone
                        , signup_panel Phone
                        ]

                _ ->
                    Element.row [ width fill, height fill ]
                        [ login_panel model.username model.password Desktop
                        , signup_panel Desktop
                        ]
    in
    page


view : Model -> TitleAndContent Msg
view model =
    { title = "Login"
    , content = render model
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BrowserEvents.onResize (\w h -> Resize w h) ]
