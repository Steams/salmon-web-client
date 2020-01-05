module Player.ArtistsView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Player.Data exposing (..)
import Styles exposing (button, edges, icon)


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


artist_album_songs album player =
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

        row song stat =
            let
                ( color, border_color ) =
                    case stat of
                        Just True ->
                            ( Styles.link_blue, Styles.link_blue )

                        Just False ->
                            ( Styles.text_grey, Styles.light_grey )

                        Nothing ->
                            ( Styles.text_black, Styles.light_grey )
            in
            Element.row
                [ width fill
                , Font.size 15
                , Font.color color
                , pointer
                , height (px 75)
                , spacing 20
                , onClick <| Play (ArtistPlaylist song.artist) song
                ]
                [ text <| String.pad 2 '0' <| String.fromInt song.number
                , text song.title
                , Element.el [ width (fillPortion 1), Border.width 1, Border.dashed, Border.color border_color ] <| text ""
                , Element.el [ alignRight ] <| text <| format_duration song.duration
                ]
    in
    Element.column [ width (fill |> maximum 1200) ]
        [ Styles.title album.name [ alignLeft, width fill ]
        , Element.column [ alignTop, height (shrink |> minimum 300), width fill ] <| List.map (\x -> row x (status x)) album.songs
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


artist_album_row player album =
    Element.row
        [ width fill
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.color Styles.light_grey
        , spacing 50
        ]
        [ artist_album_info album
        , artist_album_songs album player
        ]


artist_albums_table albums player =
    Element.column [ width fill, spacing 30, width (fill |> maximum 1400), centerX ] <|
        Element.el
            [ alignLeft
            , Font.color Styles.text_grey
            , Font.size 15
            , Font.family [ Font.typeface "Roboto" ]
            ]
            (text "ALBUMS")
            :: List.map (artist_album_row player) albums


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


artist_details_page page_height artist player =
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
        , artist_albums_table artist.albums player
        ]


artist_list_page pmodel =
    let
        available_height =
            pmodel.window.height - (70 + 100)

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


phone_artist_row artist =
    Element.row
        [ width fill
        , onClick <| SelectArtist artist
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.color Styles.light_grey
        , height (px 40)
        , spacing 10
        ]
        [ Element.column [ spacing 7, width fill ]
            [ el [ Font.bold, Font.size 14, width (fill |> maximum 300), clip ] <| text artist.name
            , Element.row [ Font.size 12, spacing 4, width fill ]
                [ Element.el [ alignLeft ] <| text <| (++) "albums : " <| String.fromInt <| List.length artist.albums
                , Element.el [ alignRight ] <| text <| format_duration artist.duration
                ]
            ]
        ]


phone_details_header artist =
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
    Element.column [ width fill ]
        [ Element.row [ height (px 200), width fill, spacing 10 ]
            [ Element.el [ centerY, height (px 200), width (px 200) ] <|
                Element.image
                    [ width (px 200)
                    , height (px 200)
                    , Border.shadow { offset = ( -3, 3 ), size = 0, blur = 8, color = Styles.light_grey }
                    , Border.rounded 200
                    ]
                    { src = "", description = "" }
            , Element.column [ centerY, height (px 200), spaceEvenly ]
                [ Element.column [ spacing 15 ]
                    [ Element.row [ Font.color Styles.text_grey ] [ Element.el [ Font.color Styles.link_blue ] <| text artist.name ]
                    ]
                , Element.row [ spacing 10 ] [ icon "music" NoOp, text <| (String.fromInt <| List.length <| List.concat <| List.map .songs artist.albums) ++ " tracks" ]
                , Element.row [ spacing 10 ] [ icon "duration" NoOp, text <| format_duration artist.duration ]
                , Element.row [ spacing 10, pointer, onClick play_action ] [ icon "play" NoOp, text "Play All" ]
                ]
            ]
        ]



phone_song_row song stat =
    let
        ( color, border_color ) =
            case stat of
                Just True ->
                    ( Styles.link_blue, Styles.link_blue )

                Just False ->
                    ( Styles.text_grey, Styles.light_grey )

                Nothing ->
                    ( Styles.text_black, Styles.light_grey )
    in
    Element.row
        [ width fill
        , Font.size 15
        , Font.color color
        , pointer
        , height (px 75)
        , spacing 20
        , onClick <| Play (ArtistPlaylist song.artist) song
        ]
        [ text <| String.pad 2 '0' <| String.fromInt song.number
        , if String.length song.title > 35 then
            Element.el [ width (px 250), clip ] <| text song.title
          else
            Element.el [ width shrink ] <| text song.title
        , Element.el [ width (fillPortion 1), Border.width 1, Border.dashed, Border.color border_color ] <| text ""
        , Element.el [ alignRight ] <| text <| format_duration song.duration
        ]

phone_album_songs_list album player =
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
    Element.column [ width (fill |> maximum 1200) ]
        [ Element.column [ alignTop, height (shrink |> minimum 300), width fill ] <| List.map (\x -> phone_song_row x (status x)) album.songs
        ]


phone_album_info album =
    Element.row
        [ width fill
        , Font.size 15
        , height (px 60)
        , spacing 10
        ]
        [ Element.column [ spacing 7, width fill ]
            [ el [ Font.bold, Font.size 20, width (fill |> maximum 300), clip ] <| text album.name
            , Element.row [ Font.size 12, spacing 4, width fill ]
                [ text <| format_duration album.duration
                , text "-"
                , text <| (String.fromInt <| List.length album.songs) ++ " tracks"
                ]
            ]
        , Element.image
            [ centerY
            , width (px 60)
            , height (px 60)
            ]
            { src = album.art, description = "" }
        ]


phone_album_row player album =
    Element.column
        [ width fill
        , Font.size 15
        , Font.color Styles.text_black
        , pointer
        , Border.color Styles.light_grey
        , spacing 10
        ]
        [ phone_album_info album
        , phone_album_songs_list album player
        ]


phone_albums_list albums player =
    Element.column [ width fill, spacing 30, width (fill |> maximum 1400), centerX ] <|
        Element.el
            [ alignLeft
            , Font.color Styles.text_grey
            , Font.size 15
            , Font.family [ Font.typeface "Roboto" ]
            ]
            (text "ALBUMS")
            :: List.map (phone_album_row player) albums


phone_details_page page_height artist player =
    let
        available_height =
            page_height - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 15, right = 15 }
        , width fill
        , height (px available_height)
        , scrollbarY
        , spacing 40
        ]
        [ phone_details_header artist
        , phone_albums_list artist.albums player
        ]


phone_artists_list artists =
    Element.column [ width fill, spacing 10 ] <| List.map phone_artist_row artists


phone_view pmodel =
    let
        artists =
            get_artists pmodel.library

        available_height =
            pmodel.window.height - (70 + 75)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 15, right = 15 }
        , width fill
        , height fill
        , height (px available_height)
        , scrollbarY
        , spacing 20
        ]
        [ Styles.title "Artists" [ alignLeft ]
        , phone_artists_list artists
        ]
