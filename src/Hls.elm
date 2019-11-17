port module Hls exposing (..)

import Json.Encode as E

port initialize : E.Value -> Cmd msg
