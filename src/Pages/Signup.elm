module Pages.Signup exposing (Model, Msg(..), init, subscriptions, update, view)

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
    , email : String
    , viewport : { width : Int, height : Int }
    }


type Msg
    = NoOp
    | UsernameInput String
    | PasswordInput String
    | EmailInput String
    | SubmitSignup
    | SignupResponse (WebData String)
    | Login
    | WindowInfo Dom.Viewport
    | Resize Int Int


init : Session -> ( Model, Cmd Msg )
init session =
    ( { username = ""
      , password = ""
      , email = ""
      , viewport = { width = 0, height = 0 }
      }
    , get_screen_info
    )



-- TODO move this to a utils module


get_screen_info =
    Task.perform WindowInfo Dom.getViewport


signup username password email =
    Api.signup SignupResponse username password email


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

        EmailInput val ->
            ( { model | email = val }
            , Cmd.none
            )

        SubmitSignup ->
            ( { model | password = "", username = "" }
            , signup model.username model.password model.email
            )

        Login ->
            ( model
            , Nav.pushUrl session.navKey (Route.toUrl Route.Login)
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View


button value handler =
    Input.button
        [ height (px 60)
        , centerX
        , Border.width 2
        , Border.color Styles.blue
        , Font.size 12
        , Font.center
        , Font.color Styles.blue
        , Font.bold
        , width (px 300)
        , Border.rounded 100
        ]
        { onPress = Just handler
        , label = text value
        }


input placeholder handler value =
    Input.text
        [ Border.width 0
        , Border.rounded 0
        , Background.color Styles.input_background
        , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
        , centerX
        , width (px 400)
        , height (px 65)
        , paddingXY 20 22
        , Font.size 20
        , Font.color Styles.text_grey
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


login_panel =
    Element.el
        [ width (fillPortion 2)
        , height fill
        , Background.color Styles.green
        , Font.color Styles.white
        ]
    <|
        Element.column [ centerY, centerX, spacing 40 ]
            [ el [ Font.size 25, Font.bold, Font.color Styles.white ] <| text "Already have an account ? Log In!"
            , paragraph [ width (px 400) ]
                [ text "To keep enjoying Salmon media streaming, click here log in "
                ]
            , button "LOG IN" Login
            ]


signup_panel username password email =
    Element.column [ width (fillPortion 3), spacing 60, centerY, centerX, paddingXY 0 30 ]
        [ el [ Font.size 25, centerX, Font.bold, Font.color Styles.green ] <| text "Create Account"
        , Element.column [ centerX, spacing 20 ]
            [ input "Username" UsernameInput username
            , input "Email" EmailInput email
            , input "Password" PasswordInput password
            ]
        , button "SIGN UP" SubmitSignup
        ]


render model =
    let
        device =
            Element.classifyDevice model.viewport

        page =
            case device.class of
                Phone ->
                    Element.column [ width fill, height fill ]
                        [ login_panel
                        , signup_panel model.username model.password model.email
                        ]

                _ ->
                    Element.row [ width fill, height fill ]
                        [ login_panel
                        , signup_panel model.username model.password model.email
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
