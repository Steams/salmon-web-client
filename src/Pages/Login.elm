module Pages.Login exposing (Model, Msg, init, update, view)

import App as App
import Browser.Navigation exposing (pushUrl)
import Api exposing (login)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Hls exposing (initialize)
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
    { username : String
    , password : String
    }




type Msg
    = NoOp
    | UsernameInput String
    | PasswordInput String
    | SubmitLogin
    | LoginResponse (WebData String)


init : Session -> ( Model, Cmd (App.Msg a))
init session =
    ( { username = "", password = "" }, Cmd.none )

login username password = Api.login (App.handle_login) username password

update : Msg -> Model -> ( Model, Cmd (App.Msg a))
update msg model =
    case msg of
        UsernameInput val ->
            ( { model | username = val }
            , Cmd.none
            )

        PasswordInput val ->
            ( { model | password = val }
            , Cmd.none
            )

        SubmitLogin ->
            ( { model | password = "",username = "" }
            , login model.username model.password
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View


render model =
    Element.column []
        [ Input.text
            [ Border.width 1
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
            , Border.rounded 0
            ]
            { onChange = to_app_msg UsernameInput
            , text = model.username
            , placeholder = Just (Input.placeholder [] <| text "Username")
            , label = Input.labelHidden ""
            }
        , Input.text
            [ Border.width 1
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
            , Border.rounded 0
            ]
            { onChange = to_app_msg PasswordInput
            , text = model.password
            , placeholder = Just (Input.placeholder [] <| text "Password")
            , label = Input.labelHidden ""
            }
        , Input.button
            [ height (px 40)
            , centerY
            , Border.width 2
            , Border.color Styles.blue
            , Font.size 12
            , Font.center
            , Font.color Styles.blue
            , Font.bold
            , width (px 100)
            ]
            { onPress = Just (app_msg SubmitLogin)
            , label = text "Login"
            }
        ]


view : Model -> TitleAndContent (App.Msg a)
view model =
    { title = "Login"
    , content = render model
    }
