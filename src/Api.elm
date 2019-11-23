module Api exposing (Song, get_data,login)

import Http as Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Encode as Encode
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Session exposing (Session)



-- import Url.Builder exposing (QueryParameter)
-- apiUrl =
--     "/api"


type Endpoint
    = GetData


type alias Credentials =
    String



-- toUrl endpoint =
--     case endpoint of
--         GetData ->
--             url [ "albums" ] []
-- TODO What about internal endpoints ?
-- url : List String -> List QueryParameter -> String
-- url paths queryParams =
--     Url.Builder.crossOrigin "https://jsonplaceholder.typicode.com" paths queryParams


type alias Song =
    { title : String
    , duration : Int
    , playlist : String
    }


song_decoder : Decoder Song
song_decoder =
    Decode.map3 Song
        (field "Title" string)
        (field "Duration" int)
        (field "Playlist" string)


song_list_decoder : Decode.Decoder (List Song)
song_list_decoder =
    Decode.list song_decoder


login : (WebData String -> msg) -> String -> String -> Cmd msg
login handler username password =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/login"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string username )
                    , ( "password", Encode.string password )
                    ]
        , expect = Http.expectJson (RemoteData.fromResult >> handler) string
        , timeout = Nothing
        , tracker = Nothing
        }


get_data : Credentials -> (WebData (List Song) -> msg) -> Cmd msg
get_data credentials handler =
    get credentials "http://localhost:8080/media" handler song_list_decoder


get : Credentials -> String -> (WebData a -> msg) -> Decoder a -> Cmd msg
get credentials endpoint handler decoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" credentials ]
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> handler) decoder
        , timeout = Nothing
        , tracker = Nothing
        }
