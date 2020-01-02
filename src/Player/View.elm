module Player.View exposing (view)

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
import Styles exposing (edges,icon)
import Task
import Tuple
import Player.Data exposing(..)
import Player.AlbumsView as AlbumsView
import Player.ArtistsView as ArtistsView
import Player.SongsView as SongsView



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
            Element.row [ height fill, spacing 15, width fill ]
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
            Element.column [ width fill] []


phone_now_playing song =
    case song of
        Just s ->
            Element.row [ height fill, spacing 15, alignLeft, width fill, clip ]
                [ Element.column [ spacing 7 ]
                    [ el [ Font.bold, Font.size 19 ] <| text s.title
                    , el [ Font.size 15 ] <| text s.album
                    ]
                ]

        _ ->
            Element.column [ width fill] []




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
        [ Element.row [alignLeft, clip, width (px 500), height fill] [now_playing song]
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


phone_controls playing seek_pos song =
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
        , height (px 70)
        , paddingXY 15 0
        , Font.color Styles.text_black
        , Border.color Styles.light_grey
        , Border.widthEach { edges | top = 1 }
        , spacing 15
        ]
        [ if playing then
            icon "pause" TogglePlay

          else
            icon "play" TogglePlay
        , phone_now_playing song
        , Element.row [ alignRight, spacing 20, Font.bold, Font.color Styles.text_grey ]
            [ icon "prev" Prev
            , icon "next" Next
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
                    seek_bar pmodel.window.width s.duration seek_pos

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


phone_player_bar pmodel =
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
                    seek_bar pmodel.window.width s.duration seek_pos

                Nothing ->
                    Element.none
    in
    Element.column
        [ height (px 75)
        , width fill
        , alignBottom
        ]
        [ seek
        , phone_controls playing seek_pos song
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
            case mode of
                Artists ->
                    True

                ViewArtist x ->
                    True

                _ ->
                    False
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


phone_topbar model mode =
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
            case mode of
                Artists ->
                    True

                ViewArtist x ->
                    True

                _ ->
                    False
    in
    Element.row
        [ height (px 70)
        , width fill
        , paddingXY 40 0
        , Border.color Styles.light_grey
        , Border.widthEach { edges | bottom = 1 }
        , spacing 70
        , Background.color Styles.white
        ]
        [ topbar_option "Songs" (ChangeMode Songs) isSongs
        , topbar_option "Albums" (ChangeMode Albums) isAlbums
        , topbar_option "Artists" (ChangeMode Artists) isArtists
        ]


phone_view : Mode -> PageModel -> Element Msg
phone_view mode pmodel =
    case mode of
        Albums ->
            Element.column
                [ height fill
                , width fill
                , Background.color Styles.white
                ]
                [ phone_topbar pmodel mode
                , AlbumsView.phone_view pmodel
                ]

        ViewAlbum album ->
            Element.column
                [ height fill
                , width fill
                , Background.color Styles.white
                ]
                [ phone_topbar pmodel mode
                , AlbumsView.phone_details_page pmodel.window.height album pmodel.player
                ]

        Artists ->
            Element.column
                [ height fill
                , width fill
                , Background.color Styles.white
                ]
                [ phone_topbar pmodel mode
                , ArtistsView.phone_view pmodel
                ]

        ViewArtist artist ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , Background.color Styles.white
                ]
                [ phone_topbar pmodel mode
                , ArtistsView.phone_details_page pmodel.window.height artist pmodel.player
                ]
        _ ->
            Element.column
                [ height fill
                , width fill
                , Background.color Styles.white
                ]
                [ phone_topbar pmodel mode
                , SongsView.phone_song_list_page pmodel
                ]


desktop_view : Mode -> PageModel -> Element Msg
desktop_view mode pmodel =
    case mode of
        Songs ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , SongsView.song_list_page pmodel
                ]

        Albums ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , AlbumsView.desktop_view pmodel
                ]

        ViewAlbum album ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , AlbumsView.album_details_page pmodel.window.height album pmodel.player
                ]

        Artists ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , ArtistsView.artist_list_page pmodel
                ]

        ViewArtist artist ->
            Element.column
                [ height fill
                , width fill
                , paddingXY 0 0
                , Background.color Styles.white
                ]
                [ topbar pmodel mode
                , ArtistsView.artist_details_page pmodel.window.height artist pmodel.player
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
                    PageModel model.window model.player library

                device =
                    Element.classifyDevice model.window

                _ =
                    Debug.log "Device" device

                page_elements =
                    case device.class of
                        Phone ->
                            [ phone_view model.mode pmodel ]

                        _ ->
                            [ -- side_panel
                              desktop_view model.mode pmodel
                            ]

                player_element =
                    case device.class of
                        Phone ->
                            phone_player_bar pmodel

                        _ ->
                            player_bar pmodel
            in
            Element.row
                [ width fill
                , height fill
                , Font.family [ Font.typeface "Open Sans" ]
                , inFront <| player_element
                ]
                page_elements

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
