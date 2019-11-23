module App exposing (Msg(..),handle_login,PageMsg(..),GlobalMsg(..))

import Browser
import RemoteData exposing (RemoteData(..), WebData)
import Url exposing (Url)


type Msg a
    = Global GlobalMsg
    | LocalMsg (PageMsg a)


type GlobalMsg
    = ChangeUrl Url
    | RequestUrl Browser.UrlRequest
    | LoginResponse (WebData String)


type PageMsg a
    = HomeMsg a
    | LoginMsg a



-- type Msg a
--     = LoginResponse (WebData String)
--     | LocalMsg a
-- type GlobalMsgs =
-- local a =
--     LocalMsg a


handle_login =
    LoginResponse >> Global
