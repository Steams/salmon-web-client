module Player.SongsView exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Player.Data exposing (..)
import Styles exposing (edges)


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


phone_songs_row : IndexedSong -> Maybe Bool -> Element Msg
phone_songs_row song status =
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
        , Border.color Styles.light_grey
        , height (px 40)
        , spacing 10
        ]
        [ Element.image
            [ centerY
            , width (px 40)
            , height (px 40)
            ]
            { src = song.art, description = "" }
        , Element.column [ spacing 7, width fill]
            [ el [ Font.bold, Font.size 14, width (fill |> maximum 300), clip] <| text song.title
            , Element.row [ Font.size 12, spacing 4, width fill ]
                [ text <| song.artist
                , text "-"
                , text song.album
                , Element.el [ alignRight, width (px 100) ] <| text <| format_duration song.duration
                ]
            ]
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
            pmodel.window.height - (70 + 100)
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


phone_songs_list songs player =
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
    Element.column [ width fill, spacing 20 ] <| List.map (\s -> phone_songs_row s (status s)) songs


phone_song_list_page pmodel =
    let
        available_height =
            pmodel.window.height - (70 + 100)
    in
    Element.column
        [ paddingEach { edges | top = 40, left = 15, right = 15 }
        , width fill
        , height (px available_height)
        , scrollbarY
        , spacing 20
        ]
        [ Styles.title "Songs" [ alignLeft ]
        , phone_songs_list pmodel.library pmodel.player
        ]
