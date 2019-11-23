module Pages.Login exposing (Model, Msg, init, update, view)

import Api exposing (Song, get_data)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Events exposing (onClick)
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
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



-- State


type Msg
    = NoOp
    | UsernameInput String
    | PasswordInput String


init : Session -> ( Model, Cmd Msg )
init session =
    ( { username = "", password = "" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
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

        _ ->
            ( model
            , Cmd.none
            )



-- View


render model =
    Element.column []
        [ Input.text
            [ Border.width 0
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
            , Border.rounded 0
            ]
            { onChange = UsernameInput
            , text = model.username
            , placeholder = Nothing
            , label = Input.labelHidden ""
            }
        ]


view : Model -> TitleAndContent Msg
view model =
    { title = "Login"
    , content = render model
    }
