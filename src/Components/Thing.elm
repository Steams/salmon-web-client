module Components.Thing exposing (Model, Msg, init, update, view)

import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData)
import Route
import Session exposing (Session)
import Styles



-- Model
-- { session : Session, contacts : List Contact }


type alias Model =
    { content : String }



-- State


type Msg
    = NoOp


init : Session -> String -> ( Model, Cmd Msg )
init session content =
    ( { content =  content }, load_data session )


load_data session =
    Cmd.none



-- Api.sendQuery session GotContacts getContacts


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- View


view : Model -> TitleAndContent Msg
view model =
    { title = "Contacts"
    , content =
        Element.column Styles.home_page
            [ text model.content
            ]
    }
