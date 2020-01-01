module Player.Data exposing (..)

import Api exposing (Song)
import Array
import Browser.Dom as Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import List.Extra as ListExtra exposing (find, findIndex, getAt, init, last)
import RemoteData exposing (RemoteData(..), WebData)
import Set as Set
import Styles exposing (edges)
import Task



-- Model
-- impossible states
-- cant be playing without an active song, PlayerState = Maybe Active Playing
-- cant have a active song without a playlist
-- cant have a seek pos without an active song
-- active must be a member of current playlist, current playlist cant be empty while active exists
-- active must be a member of current playlist, current playlist cant be empty while active exists


type alias Model =
    { data : WebData (List Song)
    , mode : Mode
    , window : Size
    , player : Maybe Player
    }


type alias PageModel =
    { window : Size
    , player : Maybe Player
    , library : List IndexedSong
    }


type alias Size =
    { width : Int, height : Int }


type alias Player =
    { current_playlist : Playlist
    , seek_pos : Float --this is a percentage of song duration
    , playing : Bool
    }


type alias Playlist =
    { prev : List Int, active : Int, next : List Int }


type alias Album =
    { name : String
    , songs : List IndexedSong
    , duration : Int
    , artist : String
    , art : String
    }


type alias Artist =
    { name : String
    , albums : List Album
    , duration : Int
    }


type alias IndexedSong =
    { index : Int
    , title : String
    , artist : String
    , album : String
    , duration : Int
    , number : Int
    , art : String
    , playlist : String
    }


type Msg
    = NoOp
    | HandleData (WebData (List Song))
    | Play PlaylistType IndexedSong
    | Next
    | Prev
    | TogglePlay
    | Nope String
    | ChangeMode Mode
    | SelectAlbum Album
    | SelectArtist Artist
    | WindowInfo Dom.Viewport
    | Seek ( Float, Float )
    | Resize Int Int
    | PlaybackEnded Bool
    | Playtime Float


type Mode
    = Songs
    | Albums
    | Artists
    | ViewAlbum Album
    | ViewArtist Artist


type PlaylistType
    = All
    | AlbumPlaylist String
    | ArtistPlaylist String


flatten_playlist { prev, active, next } =
    prev ++ [ active ] ++ next


to_index i { title, artist, album, duration, number, art, playlist } =
    IndexedSong i title artist album duration number art playlist


index_songs songs =
    List.indexedMap to_index songs


get_albums : List IndexedSong -> List Album
get_albums library =
    let
        album_names =
            Set.toList <| Set.fromList <| List.map .album library

        name_to_album name =
            let
                album_songs =
                    List.sortBy .number <| List.filter ((==) name << .album) library
            in
            { name = name
            , songs = album_songs
            , duration = List.foldl (+) 0 (List.map .duration album_songs)
            , artist = List.foldl (\a b -> a.artist) "" album_songs
            , art = List.foldl (\a b -> a.art) "" album_songs
            }

        _ =
            Debug.log "get albums" (List.map name_to_album album_names)
    in
    List.map name_to_album album_names


get_album : String -> List IndexedSong -> Maybe Album
get_album name library =
    let
        album_songs =
            library
                |> List.filter (\x -> x.album == name)
                |> List.sortBy .number

        -- List.map .index <| List.sortBy .number <| List.filter ((==) name << .album) library
    in
    case album_songs of
        [] ->
            Nothing

        _ ->
            Just
                { name = name
                , songs = album_songs
                , duration = List.foldl (+) 0 (List.map .duration album_songs)
                , artist = List.foldl (\a b -> a.artist) "" album_songs
                , art = List.foldl (\a b -> a.art) "" album_songs
                }


get_artists songs =
    let
        artist_names =
            Set.toList <| Set.fromList <| List.map .artist songs

        name_to_artist name =
            let
                artist_songs =
                    List.filter ((==) name << .artist) songs
            in
            { name = name
            , duration = List.foldl (+) 0 (List.map .duration artist_songs)
            , albums = List.filter ((==) name << .artist) (get_albums songs)

            -- , art = List.foldl (\a b -> a.art) "" artist_songs
            }
    in
    List.map name_to_artist artist_names


get_artist : String -> List IndexedSong -> Maybe Artist
get_artist name library =
    let
        artist_album_names : List String
        artist_album_names =
            library
                |> List.filter (\x -> x.artist == name)
                |> List.map .album
                |> Set.fromList
                |> Set.toList

        artist_albums : List Album
        artist_albums =
            List.filterMap (\x -> get_album x library) artist_album_names
    in
    case artist_albums of
        [] ->
            Nothing

        _ ->
            Just
                { name = name
                , albums = artist_albums
                , duration = List.foldl (+) 0 (List.map .duration artist_albums)

                -- , art = List.foldl (\a b -> a.art) "" album_songs
                }


empty_song =
    IndexedSong -1 "" "" "" -1 -1 "" ""


format_duration time =
    let
        seconds =
            modBy 60 time

        minutes =
            modBy 60 (time // 60)

        hours =
            time // 3600
    in
    case hours of
        0 ->
            String.fromInt minutes
                ++ ":"
                ++ (if seconds > 9 then
                        String.fromInt seconds

                    else
                        "0" ++ String.fromInt seconds
                   )

        _ ->
            String.fromInt hours ++ " hr " ++ String.fromInt minutes ++ " min"


is_song_equal : Song -> Song -> Bool
is_song_equal a b =
    a.album == b.album && a.title == b.title


playlist_contains : List Int -> List Song -> Song -> Bool
playlist_contains playlist library song =
    let
        with_pos =
            Array.toIndexedList <| Array.fromList library

        songs =
            List.map Tuple.second <| List.filter (\( x, y ) -> List.member x playlist) with_pos

        result =
            case find (is_song_equal song) songs of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    result


song_index playlist library song =
    let
        with_pos =
            Array.toIndexedList <| Array.fromList library

        songs =
            List.map Tuple.second <| List.filter (\( x, y ) -> List.member x playlist) with_pos

        result =
            case findIndex (is_song_equal song) songs of
                Just i ->
                    i

                Nothing ->
                    -1
    in
    result


get_song playlist library index =
    let
        library_index =
            Maybe.withDefault -1 <| getAt index playlist
    in
    getAt library_index library
