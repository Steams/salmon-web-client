module Player exposing (Model, Msg, init, subscriptions, update, view)

import Player.State as State exposing (..)
import Player.Data as Data exposing (..)
import Player.View as View exposing (..)



-- Types


type alias Msg =
    Data.Msg


type alias Model =
    Data.Model



-- State


init =
    State.init


update =
    State.update


subscriptions : Model -> Sub Msg
subscriptions =
    State.subscriptions



-- View

view = View.view
