module Player.AlbumsView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Player.Data exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Styles exposing (button, edges, icon)


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


album_songs_row_item value =
    Element.el [ clipX, width <| fillPortion 1, height (px 30) ] <|
        Element.el [ width (fill |> maximum 330), clip ] <|
            text value


album_songs_row song status =
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
        , onClick <| Play (AlbumPlaylist song.album) song
        , Font.size 15
        , Font.color color
        , pointer
        , Border.widthEach { edges | bottom = 1 }
        , Border.color Styles.light_grey
        , height (px 40)
        ]
        [ album_songs_row_item <| String.pad 2 '0' <| String.fromInt song.number
        , album_songs_row_item song.title
        , album_songs_row_item song.artist
        , album_songs_row_item song.album
        , album_songs_row_item "5 Days Ago"
        , album_songs_row_item <| format_duration song.duration
        ]


album_songs_table songs player =
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
    Element.column [ width fill, spacing 20 ] <| List.map (\x -> album_songs_row x (status x)) songs


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


album_details_page page_height album player =
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
        , album_songs_table album.songs player
        ]


desktop_view pmodel =
    let
        albums =
            get_albums pmodel.library

        available_height =
            pmodel.window.height - (70 + 100)
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


-- phone_details_header album =
--     let
--         play_action =
--             case album.songs of
--                 [] ->
--                     NoOp

--                 x :: xs ->
--                     Play (AlbumPlaylist album.name) x
--     in
--     Element.column [ height (px 400), width fill, spacing 20 ]
--         [ Element.el [ height (px 300), width fill] <|
--             Element.image
--                 [ centerY
--                 , centerX
--                 , width (px 200)
--                 , height (px 200)
--                 , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
--                 ]
--                 { src = album.art, description = "" }
--         , Styles.title album.name [ centerX ]
--         , Element.row
--             [ Font.color Styles.text_grey
--             , centerX
--             ]
--             [ text " BY : "
--             , Element.el [ Font.color Styles.link_blue ] <| text album.artist
--             ]
--         , Element.el [centerX, width (px 200)] <| button "PLAY" play_action
--         ]

phone_details_header album =
    let
        play_action =
            case album.songs of
                [] ->
                    NoOp

                x :: xs ->
                    Play (AlbumPlaylist album.name) x
    in
    Element.row [ height (px 400), width fill, spacing 40 ]
        [ Element.el [ height (px 200) ] <|
            Element.image
                [ centerY
                , width (px 200)
                , height (px 200)
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




phone_details_page page_height album player =
    let
        available_height =
            page_height - (70 + 100)
    in
    Element.column
        [
         width fill
        , height fill
        , height (px available_height)
        , scrollbarY
        ]
        [ phone_details_header album
        ]


phone_album_row album =
    Element.row
        [ width fill
        , onClick <| SelectAlbum album
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.color Styles.light_grey
        , height (px 40)
        , spacing 10
        ]
        [ Element.image
            [ centerY
            , width (px 40)
            , height (px 40)
            ]
            { src = album.art, description = "" }
        , Element.column [ spacing 7, width fill ]
            [ el [ Font.bold, Font.size 14, width (fill |> maximum 300), clip ] <| text album.name
            , Element.row [ Font.size 12, spacing 4, width fill ]
                [ text <| album.artist
                , Element.el [ alignRight, width (px 100) ] <| text <| format_duration album.duration
                ]
            ]
        ]


phone_album_list albums =
    Element.column [ width fill ] <| List.map phone_album_row albums


phone_view pmodel =
    let
        albums =
            get_albums pmodel.library

        available_height =
            pmodel.window.height - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 50, right = 50 }
        , width fill
        , height fill
        , height (px available_height)
        , scrollbarY
        ]
        [ Styles.title "Albums" [ alignLeft ]
        , phone_album_list albums
        ]
