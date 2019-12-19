module Pages.Home exposing (Model, Msg, init, subscriptions, update, view)

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
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Set as Set
import Styles exposing (edges)
import Task
import Tuple



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
    , page_size : Size
    , player : Maybe Player
    }


type alias PageModel =
    { size : Size
    , player : Maybe Player
    , library : List IndexedSong
    }


type alias Size =
    { x : Int, y : Int }


type alias Player =
    { current_playlist : Playlist
    , seek_pos : Float --this is a percentage of song duration
    , playing : Bool
    }


type alias Playlist =
    { prev : List Int, active : Int, next : List Int }


flatten_playlist { prev, active, next } =
    prev ++ [ active ] ++ next


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



-- Model will need idea of currently displayed list of songs, and active should be an index so you can next/previous
-- State


type Msg
    = NoOp
    | HandleData (WebData (List Song))
    | Play PlaylistType IndexedSong
      -- | PlayAlbumSong Album Song
      -- | PlayAlbum Album
      -- | PlayArtist Artist
    | Next
    | Prev
      -- | LoadSong Song
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


init : Session -> ( Model, Cmd Msg )
init session =
    ( { data = Loading
      , mode = Songs
      , page_size = Size 0 0
      , player = Nothing
      }
    , Cmd.batch [ get_screen_info, load_data session ]
    )


get_screen_info =
    Task.perform WindowInfo Dom.getViewport


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



-- init : Session -> ( Model, Cmd Msg )
-- init session =
--     ( { data =
--             Success
--                 [ Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Fireworks (Feat. Alicia Keys)" "Drake" "Album name" 311 "http://localhost:3000/fireworks.jpg" "http://localhost:3000/fireworks.m3u8"
--                 , Song "Fireworks (Feat. Alicia Keys)" "Drake" "Album name" 311 "http://localhost:3000/fireworks.jpg" "http://localhost:3000/fireworks.m3u8"
--                 , Song "G.O.O.D. Friday (ft. Common, Pusha T, Kid Cudi, Big Sean & Charlie Wilson of The Gap Band)" "Kayne Quest" "Good Friday" 311 "http://localhost:3000/goodfriday.jpg" "http://localhost:3000/goodfriday.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Fireworks (Feat. Alicia Keys)" "Drake" "Album name" 311 "http://localhost:3000/fireworks.jpg" "http://localhost:3000/fireworks.m3u8"
--                 , Song "G.O.O.D. Friday (ft. Common, Pusha T, Kid Cudi, Big Sean & Charlie Wilson of The Gap Band)" "Kayne Quest" "Good Friday" 311 "http://localhost:3000/goodfriday.jpg" "http://localhost:3000/goodfriday.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "G.O.O.D. Friday (ft. Common, Pusha T, Kid Cudi, Big Sean & Charlie Wilson of The Gap Band)" "Kayne Quest" "Good Friday" 311 "http://localhost:3000/goodfriday.jpg" "http://localhost:3000/goodfriday.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Fireworks (Feat. Alicia Keys)" "Drake" "Album name" 311 "http://localhost:3000/fireworks.jpg" "http://localhost:3000/fireworks.m3u8"
--                 , Song "G.O.O.D. Friday (ft. Common, Pusha T, Kid Cudi, Big Sean & Charlie Wilson of The Gap Band)" "Kayne Quest" "Good Friday" 311 "http://localhost:3000/goodfriday.jpg" "http://localhost:3000/goodfriday.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "G.O.O.D. Friday (ft. Common, Pusha T, Kid Cudi, Big Sean & Charlie Wilson of The Gap Band)" "Kayne Quest" "Good Friday" 311 "http://localhost:3000/goodfriday.jpg" "http://localhost:3000/goodfriday.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 , Song "Closer (Feat. Andreena Mill)" "Drake" "Comeback Season" 311 "http://localhost:3000/closer.jpg" "http://localhost:3000/closer.m3u8"
--                 ]
--       , active = Just (Song "Fireworks (Feat. Alicia Keys)" "Drake" "Album name" 311 "http://localhost:3000/fireworks.jpg" "http://localhost:3000/fireworks.m3u8")
--       , playing = False
--       , mode = Albums
--       , page_height = 0
--       }
--     , get_screen_info
--     )


empty_song =
    IndexedSong -1 "" "" "" -1 -1 "" ""


load_data session =
    get_data session.csrfToken HandleData


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
              -- TODO change song.playlist to song.url
            , Ports.initialize (E.string song.playlist)
            )

        -- PlayAlbum album ->
        --     let
        --         library =
        --             RemoteData.withDefault [] model.data
        --         playlist =
        --             List.map (\x -> Maybe.withDefault -1 (findIndex (is_song_equal x) library)) album.songs
        --         index =
        --             0
        --         song =
        --             Maybe.withDefault empty_song <| get_song playlist library index
        --     in
        --     ( { model | active = Just index, playing = True, current_playlist = Just playlist }
        --     , Ports.initialize (E.string song.playlist)
        --     )
        -- PlayArtist artist ->
        --     let
        --         library =
        --             RemoteData.withDefault [] model.data
        --         -- TODO need to sort album songs in track order
        --         songs =
        --             List.concat <| List.map (\x -> x.songs) artist.albums
        --         playlist =
        --             List.map (\x -> Maybe.withDefault -1 (findIndex (is_song_equal x) library)) songs
        --         index =
        --             0
        --         song =
        --             Maybe.withDefault empty_song <| get_song playlist library index
        --     in
        --     ( { model | active = Just index, playing = True, current_playlist = Just playlist }
        --     , Ports.initialize (E.string song.playlist)
        --     )
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
                                    ( Just <| Player (Playlist prev active next) 0 True, Ports.initialize (E.string song.playlist) )

                        Nothing ->
                            ( Nothing, Cmd.none )
            in
            ( { model | player = new_player }, cmd )

        Seek ( x, y ) ->
            let
                seek_pos =
                    x / toFloat model.page_size.x

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
                    Debug.log "" info
            in
            ( { model | page_size = Size (round info.viewport.height) (round info.viewport.width) }
            , Cmd.none
            )

        Resize w h ->
            let
                _ =
                    Debug.log "size " h
            in
            ( { model | page_size = Size w h }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )



-- View


logo =
    Element.el [ height (px 100), width fill, Font.size 15, Font.color Styles.white ] <| Element.el [ centerY, centerX ] <| text "LOGO"


side_panel =
    Element.column
        [ height fill
        , width (px 300)
        , Background.color Styles.dark_blue
        ]
        [ logo
        , Element.el [ width fill, height (px 50), Font.color Styles.white ] <| text "Home"
        , Element.el [ width fill, height (px 50), Font.color Styles.white ] <| text "Settings"
        ]


now_playing song =
    case song of
        Just s ->
            Element.row [ height fill, spacing 15, alignLeft, width (px 400) ]
                [ Element.image
                    [ centerY
                    , width (px 70)
                    , height (px 70)
                    , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
                    ]
                    { src = s.art, description = "" }
                , Element.column [ spacing 7 ]
                    [ el [ Font.bold, Font.size 19 ] <| text s.title
                    , el [ Font.size 15 ] <| text s.album
                    ]
                ]

        _ ->
            Element.column [ spacing 7 ] []


icon name handler =
    Element.image [ onClick handler, pointer, centerY, width (px 25), height (px 25) ] { src = "http://localhost:9000/" ++ name ++ ".png", description = "" }


controls : Bool -> Float -> Maybe IndexedSong -> Element Msg
controls playing seek_pos song =
    let
        duration =
            case song of
                Just s ->
                    s.duration

                Nothing ->
                    0
    in
    Element.row
        [ Background.color Styles.white
        , width fill
        , height (px 100)
        , paddingXY 100 0
        , Font.color Styles.text_black
        , Border.color Styles.light_grey
        , Border.widthEach { edges | top = 1 }
        ]
        [ now_playing song
        , Element.row [ centerX, spacing 30, Font.bold, Font.color Styles.text_grey ]
            [ icon "shuffle" NoOp
            , icon "prev" Prev
            , if playing then
                icon "pause" TogglePlay

              else
                icon "play" TogglePlay
            , icon "next" Next
            , icon "repeat" NoOp
            ]
        , Element.row [ alignRight, spacing 30 ]
            [ text <| format_duration (floor (seek_pos * toFloat duration)) ++ " / " ++ format_duration duration
            , icon "volume" NoOp
            , text "----------------"
            ]
        ]


seek_bar page_width duration seek_pos =
    let
        seek_location =
            round (toFloat page_width * seek_pos)
    in
    Element.row
        [ width fill
        , height (px 4)
        , paddingXY 1 0
        , Background.color Styles.light_grey
        , pointer
        , Element.htmlAttribute <| Mouse.onClick (\event -> Seek event.offsetPos)
        ]
        [ Element.el
            [ height fill
            , width (px seek_location |> maximum (page_width - 5))
            , Background.color Styles.link_blue
            ]
          <|
            Element.none
        ]


player_bar pmodel =
    let
        index =
            Maybe.map (.active << .current_playlist) pmodel.player

        song : Maybe IndexedSong
        song =
            index |> Maybe.andThen (\x -> getAt x pmodel.library)

        seek_pos =
            Maybe.withDefault 0 <| Maybe.map .seek_pos pmodel.player

        playing =
            Maybe.withDefault False <| Maybe.map .playing pmodel.player

        seek =
            case song of
                Just s ->
                    seek_bar pmodel.size.x s.duration seek_pos

                Nothing ->
                    Element.none
    in
    Element.column
        [ height (px 105)
        , width fill
        , alignBottom
        ]
        [ seek
        , controls playing seek_pos song
        , html <| Html.audio [ HtmlAttribute.attribute "id" "hls-audio" ] []
        ]


topbar_option name handler active =
    let
        ( border, font ) =
            case active of
                True ->
                    ( Border.widthEach { edges | bottom = 1 }
                    , Font.color Styles.text_black
                    )

                False ->
                    ( Border.width 0
                    , Font.color Styles.text_grey
                    )
    in
    text name
        |> Element.el [ centerY, font ]
        |> Element.el
            [ height fill
            , border
            , font
            , pointer
            , Font.size 18
            , onClick handler
            ]


searchbar =
    Element.el [ alignRight, centerY, height (px 35) ] <|
        Input.text
            [ width (px 400)
            , height fill
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = Styles.black } ]
            , Border.width 0
            , Border.rounded 0
            , Font.size 20
            , Font.color Styles.text_grey
            , Background.color Styles.white
            ]
            { onChange = Nope
            , text = ""
            , placeholder = Just (Input.placeholder [] <| el [ centerY, Font.color (rgb255 200 200 200) ] <| text "Search songs, albums and artists...")
            , label = Input.labelHidden ""
            }


topbar model mode =
    let
        isAlbums =
            case mode of
                Albums ->
                    True

                ViewAlbum x ->
                    True

                _ ->
                    False

        isSongs =
            Songs == mode

        isArtists =
            Artists == mode
    in
    Element.row
        [ height (px 70)
        , width fill
        , Border.color Styles.light_grey
        , Border.widthEach { edges | bottom = 1 }
        , spacing 70
        , paddingXY 50 0
        , Background.color Styles.white
        ]
        [ topbar_option "Songs" (ChangeMode Songs) isSongs
        , topbar_option "Albums" (ChangeMode Albums) isAlbums
        , topbar_option "Artists" (ChangeMode Artists) isArtists
        , searchbar
        ]


button value handler =
    Input.button
        [ height (px 55)
        , width (px 150)
        , Border.rounded 100
        , Border.width 2
        , Border.color Styles.blue
        , Font.size 12
        , Font.center
        , Font.color Styles.blue
        , Font.bold
        ]
        { onPress = Just handler
        , label = text value
        }



-- #+ Artists Page


artist_table_header =
    let
        header x =
            el
                [ centerX
                , Font.size 13
                , Font.family [ Font.typeface "Roboto" ]
                , Font.color Styles.light_grey
                , Font.bold
                , width <| fillPortion 1
                , Font.alignLeft
                ]
            <|
                text x
    in
    Element.row [ width fill, height (px 60) ]
        [ Element.el [ width (px 100) ] Element.none
        , header "Name"
        , header "ARTIST"
        , header "Tracks"
        , header "Duration"
        ]


artist_row_item value =
    Element.el [ clipX, width <| fillPortion 1, height (px 20), centerY ] <|
        Element.el [ width (fill |> maximum 330), clip ] <|
            text value


artist_art value =
    Element.el [ width (px 100), height (px 75), alignBottom ] <|
        Element.image
            [ centerY
            , width (px 70)
            , height (px 70)
            , alignLeft
            ]
            { src = value, description = "" }


artist_row artist =
    Element.row
        [ width fill
        , onClick <| SelectArtist artist
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.widthEach { edges | bottom = 1 }
        , Border.color Styles.light_grey
        , height (px 75)
        ]
        [ artist_art ""
        , artist_row_item artist.name
        , artist_row_item "album count"
        , artist_row_item (String.fromInt <| List.length <| List.concat <| List.map .songs artist.albums)
        , artist_row_item <| format_duration artist.duration
        ]


artist_table artists =
    Element.column
        [ width fill
        , Border.widthEach { edges | top = 1 }
        , Border.color Styles.light_grey
        ]
    <|
        List.map artist_row artists


artist_album_songs album =
    let
        row song =
            Element.row
                [ width fill
                , Font.size 15
                , Font.color Styles.text_black
                , pointer
                , height (px 75)
                , spacing 20
                , onClick <| Play (ArtistPlaylist song.artist) song
                ]
                [ text <| String.pad 2 '0' <| String.fromInt song.number
                , text song.title
                , Element.el [ width (fillPortion 1), Border.width 1, Border.dashed, Border.color Styles.light_grey ] <| text ""
                , Element.el [ alignRight ] <| text <| format_duration song.duration
                ]
    in
    Element.column [ width (fill |> maximum 1200) ]
        [ Styles.title album.name [ alignLeft, width fill ]
        , Element.column [ alignTop, height (shrink |> minimum 300), width fill ] <| List.map row album.songs
        ]


artist_album_info album =
    Element.column
        [ width (px 250)
        , alignTop
        , spacing 15
        ]
        [ Element.image
            [ width (px 250)
            , height (px 250)
            , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
            ]
            { src = album.art, description = "" }
        , text <| "length : " ++ format_duration album.duration
        , text "release date : 12/2/2019"
        ]


artist_album_row album =
    Element.row
        [ width fill
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.color Styles.light_grey
        , spacing 50
        ]
        [ artist_album_info album
        , artist_album_songs album
        ]


artist_albums_table albums =
    Element.column [ width fill, spacing 30, width (fill |> maximum 1400), centerX ] <|
        Element.el
            [ alignLeft
            , Font.color Styles.text_grey
            , Font.size 15
            , Font.family [ Font.typeface "Roboto" ]
            ]
            (text "ALBUMS")
            :: List.map artist_album_row albums


artist_details_header artist =
    let
        play_action =
            case artist.albums of
                [] ->
                    NoOp

                album :: _ ->
                    case album.songs of
                        [] ->
                            NoOp

                        x :: _ ->
                            Play (ArtistPlaylist artist.name) x
    in
    Element.row [ height (px 400), width (fill |> maximum 1400), spacing 40, centerX ]
        [ Element.el [ height (px 300) ] <|
            Element.image
                [ centerY
                , width (px 300)
                , height (px 300)
                , Border.rounded 200
                , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
                ]
                { src = "", description = "" }
        , Element.column [ height (px 300), spaceEvenly ]
            [ Element.column [ spacing 15 ]
                [ Element.el [ Font.color Styles.text_grey, Font.size 15 ] <| text " ARTIST"
                , Styles.title artist.name []

                -- , Element.row [ Font.color Styles.text_grey ] [ text " BY : ", Element.el [ Font.color Styles.link_blue ] <| text artist.artist ]
                ]
            , Element.row [ spacing 50 ]
                [ Element.row [ spacing 10 ] [ icon "music" NoOp, text <| (String.fromInt <| List.length <| List.concat <| List.map .songs artist.albums) ++ " tracks" ]
                , Element.row [ spacing 10 ] [ icon "duration" NoOp, text <| format_duration artist.duration ]
                ]
            , button "PLAY" play_action
            ]
        ]


artist_details_page page_height artist =
    let
        available_height =
            page_height - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 50, right = 50 }
        , width fill
        , height (px available_height)
        , scrollbarY
        ]
        [ artist_details_header artist
        , artist_albums_table artist.albums
        ]


artist_list_page pmodel =
    let
        available_height =
            pmodel.size.y - (70 + 100)

        artists =
            get_artists pmodel.library
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 50, right = 50 }
        , width fill
        , height fill
        , height (px available_height)
        , scrollbarY
        ]
        [ Styles.title "Artists" [ alignLeft ]
        , artist_table_header
        , artist_table artists
        ]



-- #+ Albums Page


album_table_header =
    let
        header x =
            el
                [ centerX
                , Font.size 13
                , Font.family [ Font.typeface "Roboto" ]
                , Font.color Styles.light_grey
                , Font.bold
                , width <| fillPortion 1
                , Font.alignLeft
                ]
            <|
                text x
    in
    Element.row [ width fill, height (px 60) ]
        [ Element.el [ width (px 100) ] Element.none
        , header "Name"
        , header "ARTIST"
        , header "Tracks"
        , header "Duration"
        ]


album_row_item value =
    Element.el [ clipX, width <| fillPortion 1, height (px 20), centerY ] <|
        Element.el [ width (fill |> maximum 330), clip ] <|
            text value


album_art value =
    Element.el [ width (px 100), height (px 75), alignBottom ] <|
        Element.image
            [ centerY
            , width (px 70)
            , height (px 70)
            , alignLeft
            ]
            { src = value, description = "" }


album_row album =
    Element.row
        [ width fill
        , onClick <| SelectAlbum album
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.widthEach { edges | bottom = 1 }
        , Border.color Styles.light_grey
        , height (px 75)
        ]
        [ album_art album.art
        , album_row_item album.name
        , album_row_item album.artist
        , album_row_item (String.fromInt <| List.length album.songs)
        , album_row_item (format_duration album.duration)
        ]


album_table albums =
    Element.column [ width fill, Border.widthEach { edges | top = 1 }, Border.color Styles.light_grey ] <| List.map album_row albums


album_songs_row song =
    Element.row
        [ width fill
        , onClick <| Play (AlbumPlaylist song.album) song
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.widthEach { edges | bottom = 1 }
        , Border.color Styles.light_grey
        , height (px 40)
        ]
        [ songs_row_item <| String.fromInt song.number
        , songs_row_item song.title
        , songs_row_item song.artist
        , songs_row_item song.album
        , songs_row_item "5 Days Ago"
        , songs_row_item <| format_duration song.duration
        ]


album_songs_table songs =
    Element.column [ width fill, spacing 20 ] <| List.map (\ x -> songs_row x Nothing) songs


album_songs_table_header =
    let
        header x =
            el
                [ centerX
                , Font.size 13
                , Font.family [ Font.typeface "Roboto" ]
                , Font.color Styles.light_grey
                , Font.bold
                , width <| fillPortion 1
                , Font.alignLeft
                ]
            <|
                text x
    in
    Element.row [ width fill, height (px 60) ]
        [ header ""
        , header "TRACK"
        , header "ARTIST"
        , header "ALBUM"
        , header "ADDED"
        , header "TIME"
        ]


album_details_header album =
    let
        play_action =
            case album.songs of
                [] ->
                    NoOp

                x :: xs ->
                    Play (AlbumPlaylist album.name) x
    in
    Element.row [ height (px 400), width fill, spacing 40 ]
        [ Element.el [ height (px 300) ] <|
            Element.image
                [ centerY
                , width (px 300)
                , height (px 300)
                , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
                ]
                { src = album.art, description = "" }
        , Element.column [ height (px 300), spaceEvenly ]
            [ Element.column [ spacing 15 ]
                [ Element.el [ Font.color Styles.text_grey, Font.size 15 ] <| text " ALBUM"
                , Styles.title album.name []
                , Element.row [ Font.color Styles.text_grey ] [ text " BY : ", Element.el [ Font.color Styles.link_blue ] <| text album.artist ]
                ]
            , Element.row [ spacing 50 ]
                [ Element.row [ spacing 10 ] [ icon "music" NoOp, text <| (String.fromInt <| List.length album.songs) ++ " tracks" ]
                , Element.row [ spacing 10 ] [ icon "duration" NoOp, text <| format_duration album.duration ]
                ]
            , button "PLAY" play_action
            ]
        ]


album_details_page page_height album =
    let
        available_height =
            page_height - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 50, right = 50 }
        , width fill
        , height fill
        , height (px available_height)
        , scrollbarY
        ]
        [ album_details_header album
        , album_songs_table_header
        , album_songs_table album.songs
        ]


album_list_page pmodel =
    let
        albums =
            get_albums pmodel.library

        available_height =
            pmodel.size.y - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 50, right = 50 }
        , width fill
        , height fill
        , height (px available_height)
        , scrollbarY
        ]
        [ Styles.title "Albums" [ alignLeft ]
        , album_table_header
        , album_table albums
        ]



-- #+ Songs Page


songs_table_header =
    let
        header x =
            el
                [ centerX
                , Font.size 13
                , Font.family [ Font.typeface "Roboto" ]
                , Font.color Styles.light_grey
                , Font.bold
                , width <| fillPortion 1
                , Font.alignLeft
                ]
            <|
                text x
    in
    Element.row [ width fill, height (px 60) ]
        [ header "TRACK"
        , header "ARTIST"
        , header "ALBUM"
        , header "ADDED"
        , header "TIME"
        ]


songs_row_item value =
    Element.el [ clipX, width <| fillPortion 1, height (px 30) ] <|
        Element.el [ width (fill |> maximum 330), clip ] <|
            text value


songs_row : IndexedSong -> Maybe Bool -> Element Msg
songs_row song status =
    let
        color =
            case status of
                Just True ->
                    Styles.link_blue

                Just False ->
                    Styles.text_grey

                Nothing ->
                    Styles.text_black
    in
    Element.row
        [ width fill
        , onClick <| Play All song
        , Font.size 15
        , Font.color color
        , pointer
        , Border.widthEach { edges | bottom = 1 }
        , Border.color Styles.light_grey
        , height (px 40)
        ]
        [ songs_row_item song.title
        , songs_row_item song.artist
        , songs_row_item song.album
        , songs_row_item "5 Days Ago"
        , songs_row_item (format_duration song.duration)
        ]


songs_table songs player =
    let
        status song =
            case player of
                Just { current_playlist, seek_pos, playing } ->
                    if song.index == current_playlist.active then
                        Just playing

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    Element.column [ width fill, spacing 20 ] <| List.map (\s -> songs_row s (status s)) songs


song_list_page pmodel =
    let
        available_height =
            pmodel.size.y - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 50, right = 50 }
        , width fill
        , height (px available_height)
        , scrollbarY
        ]
        [ Styles.title "Songs" [ alignLeft ]
        , songs_table_header
        , songs_table pmodel.library pmodel.player
        ]


main_panel : Mode -> PageModel -> Element Msg
main_panel mode pmodel =
    case mode of
        Songs ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , song_list_page pmodel
                ]

        Albums ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , album_list_page pmodel
                ]

        ViewAlbum album ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , album_details_page pmodel.size.y album
                ]

        Artists ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , artist_list_page pmodel
                ]

        ViewArtist artist ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , artist_details_page pmodel.size.y artist
                ]


render model =
    case model.data of
        Loading ->
            text "Loading Media Library..."

        Success [] ->
            Element.row [] [ text "Get started by setting up salmon media server on your library machine" ]

        Success songs ->
            let
                library =
                    index_songs songs

                pmodel =
                    PageModel model.page_size model.player library
            in
            Element.row
                [ width fill
                , height fill
                , Font.family [ Font.typeface "Open Sans" ]
                , inFront <| player_bar pmodel
                ]
                [ -- side_panel
                  main_panel model.mode pmodel
                ]

        Failure error ->
            let
                _ =
                    Debug.log "error" error
            in
            text "Failed to load Media Library"

        NotAsked ->
            text "Not Asked"


view : Model -> TitleAndContent Msg
view model =
    { title = "Contacts"
    , content = render model
    }


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
