port module Storage exposing (..)

import Json.Encode as E

port cache : E.Value -> Cmd msg
