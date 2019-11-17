module Api exposing (Song, get_data)

import Http as Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Session exposing (Session)



-- import Url.Builder exposing (QueryParameter)
-- apiUrl =
--     "/api"


type Endpoint
    = GetData



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


get_data : (WebData (List Song) -> msg) -> Cmd msg
get_data handler =
    get "http://localhost:8080/media" handler song_list_decoder



-- get : Endpoint -> (WebData a -> msg) -> Decoder a -> Cmd msg


get : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
get endpoint handler decoder =
    Http.get
        { url = endpoint
        , expect = Http.expectJson (\x -> handler (RemoteData.fromResult x)) decoder
        }
