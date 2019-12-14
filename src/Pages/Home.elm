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
import List.Extra exposing (find, findIndex, getAt)
import Ports as Ports
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Set as Set
import Styles exposing (edges)
import Task
import Tuple



-- Model


type alias Model =
    { data : WebData (List Song)
    , active : Maybe Int
    , playing : Bool
    , mode : Mode
    , page_height : Int
    , page_width : Int
    , current_playlist : Maybe (List Int)
    , seek_pos : Float --this is a percentage of song duration
    }


type alias Album =
    { name : String
    , songs : List Song
    , duration : Int
    , artist : String
    , art : String
    }


type alias Artist =
    { name : String
    , songs : List Song -- TODO this property is redundant with albums
    , albums : List Album -- TODO need an album for singles
    , duration : Int
    }



-- Model will need idea of currently displayed list of songs, and active should be an index so you can next/previous
-- State


type Msg
    = NoOp
    | HandleData (WebData (List Song))
    | LoadSong Song
    | PlayAlbumSong Album Song
    | PlayAlbum Album
    | PlayArtist Artist
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


type Mode
    = Songs
    | Albums
    | Artists
    | ViewAlbum Album
    | ViewArtist Artist


init : Session -> ( Model, Cmd Msg )
init session =
    ( { data = Loading
      , active = Nothing
      , playing = False
      , mode = Songs
      , page_height = 0
      , page_width = 0
      , seek_pos = 0
      , current_playlist = Nothing
      }
    , Cmd.batch [ get_screen_info, load_data session ]
    )


get_screen_info =
    Task.perform WindowInfo Dom.getViewport


get_albums songs =
    let
        album_names =
            Set.toList <| Set.fromList <| List.map .album songs

        name_to_album name =
            let
                album_songs =
                    List.sortBy .number <| List.filter ((==) name << .album) songs
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
            , songs = artist_songs
            , duration = List.foldl (+) 0 (List.map .duration artist_songs)
            , albums = List.filter ((==) name << .artist) (get_albums songs)

            -- , art = List.foldl (\a b -> a.art) "" artist_songs
            }
    in
    List.map name_to_artist artist_names



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
    Song "" "" "" -1 -1 "" ""


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

        LoadSong song ->
            let
                library =
                    RemoteData.withDefault [] model.data

                playlist =
                    case model.current_playlist of
                        Nothing ->
                            List.range 0 (List.length library - 1)

                        Just xs ->
                            case playlist_contains xs library song of
                                True ->
                                    xs

                                False ->
                                    List.range 0 (List.length library - 1)

                index =
                    song_index playlist library song
            in
            ( { model | active = Just index, playing = True, current_playlist = Just playlist }
              -- TODO change song.playlist to song.url
            , Ports.initialize (E.string song.playlist)
            )

        PlayAlbum album ->
            let
                library =
                    RemoteData.withDefault [] model.data

                playlist =
                    List.map (\x -> Maybe.withDefault -1 (findIndex (is_song_equal x) library)) album.songs

                index =
                    0

                song =
                    Maybe.withDefault empty_song <| get_song playlist library index
            in
            ( { model | active = Just index, playing = True, current_playlist = Just playlist }
            , Ports.initialize (E.string song.playlist)
            )

        PlayArtist artist ->
            let
                library =
                    RemoteData.withDefault [] model.data

                -- TODO need to sort album songs in track order
                songs =
                    List.concat <| List.map (\x -> x.songs) artist.albums

                playlist =
                    List.map (\x -> Maybe.withDefault -1 (findIndex (is_song_equal x) library)) songs

                index =
                    0

                song =
                    Maybe.withDefault empty_song <| get_song playlist library index
            in
            ( { model | active = Just index, playing = True, current_playlist = Just playlist }
            , Ports.initialize (E.string song.playlist)
            )

        TogglePlay ->
            let
                action =
                    case model.playing of
                        False ->
                            Ports.play (E.string "")

                        True ->
                            Ports.pause (E.string "")
            in
            ( { model | playing = not model.playing }, action )

        Next ->
            case ( model.active, model.current_playlist ) of
                ( Just i, Just playlist ) ->
                    let
                        library =
                            RemoteData.withDefault [] model.data

                        active =
                            if i + 1 < List.length playlist then
                                i + 1

                            else
                                0

                        song =
                            Maybe.withDefault empty_song <| get_song playlist library active
                    in
                    ( { model | active = Just active }
                    , Ports.initialize (E.string song.playlist)
                    )

                _ ->
                    ( model, Cmd.none )

        Prev ->
            case ( model.active, model.current_playlist ) of
                ( Just i, Just playlist ) ->
                    let
                        library =
                            RemoteData.withDefault [] model.data

                        active =
                            if i - 1 >= 0 then
                                i - 1

                            else
                                0

                        song =
                            Maybe.withDefault empty_song <| get_song playlist library active
                    in
                    ( { model | active = Just active }
                    , Ports.initialize (E.string song.playlist)
                    )

                _ ->
                    ( model, Cmd.none )

        Seek ( x, y ) ->
            let
                seek_pos =
                    x / toFloat model.page_width
            in
            ( { model | seek_pos = seek_pos }
            , Ports.seek (E.float seek_pos)
            )

        PlaybackEnded bool ->
            let
                _ = Debug.log "PLayback ended"
            in
            ( model
            , Cmd.none
            )

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
            ( { model | page_height = round info.viewport.height, page_width = round info.viewport.width }
            , Cmd.none
            )

        Resize w h ->
            let
                _ =
                    Debug.log "size " h
            in
            ( { model | page_height = h, page_width = w }
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


controls song =
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
            , icon "play" TogglePlay
            , icon "next" Next
            , icon "repeat" NoOp
            ]
        , Element.row [ alignRight, spacing 30 ]
            [ text "0:00 / 0:00"
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
        , Background.color Styles.light_grey
        , pointer
        , Element.htmlAttribute <| Mouse.onClick (\event -> Seek event.offsetPos)
        ]
        [ Element.el
            [ height fill
            , width (px seek_location)
            , Background.color Styles.link_blue
            ]
          <|
            Element.none
        ]


player page_width seek_pos song =
    let
        seek =
            case song of
                Just s ->
                    seek_bar page_width s.duration seek_pos

                Nothing ->
                    Element.none
    in
    Element.column
        [ height (px 105)
        , width fill
        , alignBottom
        ]
        [ seek
        , controls song
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


topbar model =
    let
        isAlbums =
            case model.mode of
                Albums ->
                    True

                ViewAlbum x ->
                    True

                _ ->
                    False

        isSongs =
            Songs == model.mode

        isArtists =
            Artists == model.mode
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
        , artist_row_item (String.fromInt <| List.length artist.songs)
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
                , onClick <| LoadSong song
                ]
                [ text "01"
                , text song.title
                , Element.el [ width (fillPortion 1), Border.width 1, Border.dashed, Border.color Styles.light_grey ] <| text ""

                -- , Element.el [alignRight] <| text (String.fromInt song.duration)
                , Element.el [ alignRight ] <| text "4:05"
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
                [ Element.row [ spacing 10 ] [ icon "music" NoOp, text <| (String.fromInt <| List.length artist.songs) ++ " tracks" ]
                , Element.row [ spacing 10 ] [ icon "duration" NoOp, text <| format_duration artist.duration ]
                ]
            , button "PLAY" (PlayArtist artist)
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


artist_list_page page_height artists =
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
        , onClick <| LoadSong song
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
    Element.column [ width fill, spacing 20 ] <| List.map songs_row songs


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
            , button "PLAY" (PlayAlbum album)
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


album_list_page page_height albums =
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


songs_row song =
    Element.row
        [ width fill
        , onClick <| LoadSong song
        , Font.size 15
        , Font.color Styles.text_black
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


songs_table songs =
    Element.column [ width fill, spacing 20 ] <| List.map songs_row songs


song_list_page page_height songs =
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
        [ Styles.title "Songs" [ alignLeft ]
        , songs_table_header
        , songs_table songs
        ]


main_panel model =
    let
        songs =
            RemoteData.withDefault [] model.data
    in
    case model.mode of
        Songs ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar model
                , song_list_page model.page_height songs
                ]

        Albums ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar model
                , album_list_page model.page_height <| get_albums songs
                ]

        ViewAlbum album ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar model
                , album_details_page model.page_height album
                ]

        Artists ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar model
                , artist_list_page model.page_height <| get_artists songs
                ]

        ViewArtist artist ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , spacing 10
                , Background.color Styles.white
                ]
                [ topbar model
                , artist_details_page model.page_height artist
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
                    RemoteData.withDefault [] model.data

                playlist =
                    Maybe.withDefault [] model.current_playlist

                active =
                    Maybe.withDefault -1 model.active
            in
            Element.row
                [ width fill
                , height fill
                , inFront <| player model.page_width model.seek_pos <| get_song playlist library active
                , Font.family [ Font.typeface "Open Sans" ]
                ]
                [ -- side_panel
                  main_panel model
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


subscriptions : model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BrowserEvents.onResize (\w h -> Resize w h)
        , Ports.ended PlaybackEnded
        ]
