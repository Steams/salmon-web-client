module Components.Blank exposing (view)

import Element exposing (..)
import Layout exposing (TitleAndContent)


view : TitleAndContent msg
view =
    { title = ""
    , content = Element.none
    }
