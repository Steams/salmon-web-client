module Route exposing (Route(..), fromUrl, replaceUrl, toUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s, string, top)


type Route
    = Root
    | Landing
    | Player
    | Login
    | Signup
    | Verification String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Landing top
        , Parser.map Player (s "player")
        , Parser.map Login (s "login")
        , Parser.map Signup (s "signup")
        , Parser.map Verification (s "verification" </> string)
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
                Landing ->
                    []

                Player ->
                    ["player"]

                Login ->
                    [ "login" ]

                Signup ->
                    [ "signup" ]

                Verification token ->
                    [ "signup", token ]
    in
    "/" ++ String.join "/" pathSegments


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toUrl route)
