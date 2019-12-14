port module Ports exposing (..)

import Json.Encode as E

port initialize     : E.Value -> Cmd msg

port pause          : E.Value -> Cmd msg
port play           : E.Value -> Cmd msg
port seek           : E.Value -> Cmd msg
port ended : (Bool -> msg) -> Sub msg
