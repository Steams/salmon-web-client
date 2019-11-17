module Session exposing (Flags, Session, init)

import Browser.Navigation as Nav


init : Flags -> Nav.Key -> Session
init { csrfToken, sessionToken } navKey =
    { csrfToken = csrfToken, sessionToken = sessionToken, navKey = navKey }


type alias Flags =
    { csrfToken : String, sessionToken : String }


type alias Session =
    { csrfToken : String, sessionToken : String, navKey : Nav.Key }
