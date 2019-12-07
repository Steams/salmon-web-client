module Session exposing (Session, init, Flags)

import Browser.Navigation as Nav

type alias Flags = ()

init : Nav.Key -> Session
init navKey =
    { csrfToken = "", navKey = navKey }


type alias Session =
    { csrfToken : String, navKey : Nav.Key }
