module Pages.NotFound exposing (view)

import Element exposing (..)
import Layout exposing (TitleAndContent)



-- VIEW


view : { title : String, content : Element msg }
view =
    { title = "Page Not Found"
    , content = text "Not Found"
    }
