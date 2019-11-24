module Route exposing (Route(..), fromUrl, replaceUrl, toUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s, string, top)


type Route
    = Root
    | Home
    | Login
    | Signup


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home top
        , Parser.map Login (s "login")
        , Parser.map Signup (s "signup")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toUrl : Route -> String
toUrl route =
    let
        pathSegments =
            case route of
                Root ->
                    []

                Home ->
                    []

                Login ->
                    [ "login" ]
                Signup ->
                    [ "signup" ]
    in
    "/" ++ String.join "/" pathSegments


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toUrl route)
