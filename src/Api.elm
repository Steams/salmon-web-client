module Api exposing (Song, get_data,login,signup,verify)

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
    , artist : String
    , album : String
    , duration : Int
    , playlist : String
    }


song_decoder : Decoder Song
song_decoder =
    Decode.map5 Song
        (field "Title" string)
        (field "Artist" string)
        (field "Album" string)
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
        , url = "http://localhost:8080/api/login"
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

signup : (WebData String -> msg) -> String -> String -> String -> Cmd msg
signup handler username password email =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/signup"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "username", Encode.string username )
                    , ( "password", Encode.string password )
                    , ( "email", Encode.string email )
                    ]
        , expect = Http.expectJson (RemoteData.fromResult >> handler) string
        , timeout = Nothing
        , tracker = Nothing
        }

verify : (WebData String -> msg) -> String -> Cmd msg
verify handler token  =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/api/verify"
        , body =
            Http.jsonBody <| Encode.string token
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
        -- , headers = [ Http.header "Authorization" credentials ]
        , headers = [ Http.header "Authorization" "1" ]
        , url = endpoint
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> handler) decoder
        , timeout = Nothing
        , tracker = Nothing
        }
