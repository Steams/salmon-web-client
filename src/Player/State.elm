module Player.State exposing (init, subscriptions, update)

import Api exposing (Song, get_data)
import Array
import Browser.Dom as Dom
import Browser.Events as BrowserEvents
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as HtmlAttribute
import Html.Events.Extra.Mouse as Mouse
import Json.Encode as E
import Layout exposing (TitleAndContent)
import List.Extra as ListExtra exposing (find, findIndex, getAt, init, last)
import Player.AlbumsView exposing (..)
import Player.ArtistsView exposing (..)
import Player.Data exposing (..)
import Player.SongsView exposing (..)
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Set as Set
import Styles exposing (edges)
import Task
import Tuple


init : Session -> ( Model, Cmd Msg )
init session =
    ( { data = Loading
      , mode = Songs
      , window = Size 0 0
      , player = Nothing
      }
    , Cmd.batch [ get_screen_info, load_data session ]
    )


load_data session =
    get_data session.csrfToken HandleData


get_screen_info =
    Task.perform WindowInfo Dom.getViewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleData res ->
            ( { model | data = res }
            , Cmd.none
            )

        Play playlist_type song ->
            let
                library =
                    List.indexedMap to_index <| RemoteData.withDefault [] model.data

                playlist : Maybe Playlist
                playlist =
                    case playlist_type of
                        All ->
                            Just
                                { prev = List.map .index <| List.take song.index library
                                , active = song.index
                                , next = List.map .index <| List.drop (song.index + 1) library
                                }

                        AlbumPlaylist name ->
                            let
                                songs : Maybe (List IndexedSong)
                                songs =
                                    Maybe.map .songs <| get_album name library

                                _ =
                                    Debug.log "album songs" songs

                                position : Maybe Int
                                position =
                                    songs |> Maybe.andThen (findIndex (\x -> x.title == song.title))

                                makeplaylist xs pos =
                                    { prev = xs |> List.take pos |> List.map .index
                                    , active = song.index
                                    , next = xs |> List.drop (pos + 1) |> List.map .index
                                    }
                            in
                            Maybe.map2 makeplaylist songs position

                        ArtistPlaylist name ->
                            let
                                songs : Maybe (List IndexedSong)
                                songs =
                                    get_artist name library
                                        |> Maybe.map .albums
                                        |> Maybe.map (List.concatMap .songs)

                                position : Maybe Int
                                position =
                                    songs |> Maybe.andThen (findIndex (\x -> x.title == song.title))

                                makeplaylist xs pos =
                                    { prev = xs |> List.take pos |> List.map .index
                                    , active = song.index
                                    , next = xs |> List.drop (pos + 1) |> List.map .index
                                    }
                            in
                            Maybe.map2 makeplaylist songs position

                -- TODO should i set playing to false, and when js tells me audio started to play set to true ?
                model_player =
                    Maybe.map (\x -> Player x 0 True) playlist
            in
            ( { model | player = model_player }
              -- TODO change song.playlist to song.url to avoid confusion
            , Ports.initialize (E.string song.playlist)
            )

        TogglePlay ->
            let
                action =
                    case model.player of
                        Just { current_playlist, seek_pos, playing } ->
                            if playing then
                                Ports.pause (E.string "")

                            else
                                Ports.play (E.string "")

                        Nothing ->
                            Cmd.none

                new_player =
                    Maybe.map
                        (\{ current_playlist, seek_pos, playing } ->
                            Player current_playlist seek_pos (not playing)
                        )
                        model.player
            in
            ( { model | player = new_player }, action )

        PlaybackEnded bool ->
            let
                library =
                    List.indexedMap to_index <| RemoteData.withDefault [] model.data

                ( new_player, cmd ) =
                    case model.player of
                        Just p ->
                            case p.current_playlist.next of
                                [] ->
                                    ( Just <| Player p.current_playlist p.seek_pos False, Cmd.none )

                                x :: xs ->
                                    let
                                        prev =
                                            p.current_playlist.prev ++ [ p.current_playlist.active ]

                                        active =
                                            x

                                        next =
                                            xs

                                        song =
                                            Maybe.withDefault empty_song <| getAt active library
                                    in
                                    ( Just <| Player (Playlist prev active next) 0 True, Ports.initialize (E.string song.playlist) )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            ( { model | player = new_player }, cmd )

        Next ->
            let
                library =
                    List.indexedMap to_index <| RemoteData.withDefault [] model.data

                ( new_player, cmd ) =
                    case model.player of
                        Just p ->
                            case p.current_playlist.next of
                                [] ->
                                    ( Just <| Player p.current_playlist p.seek_pos False, Cmd.none )

                                x :: xs ->
                                    let
                                        prev =
                                            p.current_playlist.prev ++ [ p.current_playlist.active ]

                                        active =
                                            x

                                        next =
                                            xs

                                        song =
                                            Maybe.withDefault empty_song <| getAt active library
                                    in
                                    ( Just <| Player (Playlist prev active next) 0 True, Ports.initialize (E.string song.playlist) )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            ( { model | player = new_player }, cmd )

        Prev ->
            let
                -- TODO you can probably just define library at the top of this entire update case statement
                library =
                    List.indexedMap to_index <| RemoteData.withDefault [] model.data

                ( new_player, cmd ) =
                    case model.player of
                        Just p ->
                            case p.current_playlist.prev of
                                [] ->
                                    ( Just <| { p | playing = False }, Cmd.none )

                                xs ->
                                    let
                                        prev =
                                            Maybe.withDefault [] <| ListExtra.init p.current_playlist.prev

                                        active =
                                            Maybe.withDefault -1 <| last xs

                                        next =
                                            p.current_playlist.active :: p.current_playlist.next

                                        song =
                                            Maybe.withDefault empty_song <| getAt active library
                                    in
                                    -- NOTE If user clicks back during the first 10% of a song, go to previous, otherwise just restart song
                                    if p.seek_pos < 0.1 then
                                        ( Just <| Player (Playlist prev active next) 0 True, Ports.initialize (E.string song.playlist) )

                                    else
                                        ( Just <| Player p.current_playlist 0 True, Ports.seek (E.float 0) )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            ( { model | player = new_player }, cmd )

        Seek ( x, y ) ->
            let
                seek_pos =
                    x / toFloat model.window.width

                new_player =
                    Maybe.map (\p -> { p | seek_pos = seek_pos }) model.player
            in
            ( { model | player = new_player }
            , Ports.seek (E.float seek_pos)
            )

        Playtime time ->
            case model.player of
                Just p ->
                    let
                        library =
                            List.indexedMap to_index <| RemoteData.withDefault [] model.data

                        song =
                            Maybe.withDefault empty_song <| getAt p.current_playlist.active library

                        seek =
                            time / toFloat song.duration

                        new_player =
                            { p | seek_pos = seek }
                    in
                    ( { model | player = Just new_player }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeMode mode ->
            ( { model | mode = mode }
            , Cmd.none
            )

        SelectAlbum album ->
            ( { model | mode = ViewAlbum album }
            , Cmd.none
            )

        SelectArtist artist ->
            ( { model | mode = ViewArtist artist }
            , Cmd.none
            )

        WindowInfo info ->
            let
                _ =
                    Debug.log "init window" info
            in
            ( { model | window = Size (round info.viewport.width) (round info.viewport.height) }
            , Cmd.none
            )

        Resize w h ->
            let
                _ =
                    Debug.log "width " w

                _ =
                    Debug.log "height " h
            in
            ( { model | window = Size w h }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        playtime =
            case model.player of
                Just { current_playlist, seek_pos, playing } ->
                    if playing then
                        Ports.playtime Playtime

                    else
                        Sub.none

                _ ->
                    Sub.none
    in
    Sub.batch
        [ BrowserEvents.onResize (\w h -> Resize w h)
        , Ports.ended PlaybackEnded
        , playtime
        ]
