module Pages.Home exposing (Model, Msg, init, update, view)

import Api exposing (Song, get_data)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Hls exposing (initialize, pause)
import Html as Html
import Html.Attributes as HtmlAttribute
import Json.Encode as E
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles exposing (edges)



-- Model


type alias Model =
    { data : WebData (List Song), active : Maybe Song }



-- State


type Msg
    = NoOp
    | HandleData (WebData (List Song))
    | LoadSong Song
    | Pause
    | Play


init : Session -> ( Model, Cmd Msg )
init session =
    ( { data = Loading, active = Nothing }, load_data session )



-- init : Session -> ( Model, Cmd Msg )
-- init session =
--     ( { data =
--             Success
--                 [ Song "Closer (Feat. Andreena Mill)"
--                     311
--                     "http://localhost:3000/closer.m3u8"
--                 , Song
--                     "Fireworks (Feat. Alicia Keys)"
--                     311
--                     "http://localhost:3000/fireworks.m3u8"
--                 , Song
--                     "G.O.O.D. Friday (ft. Common, Pusha T, Kid Cudi, Big Sean & Charlie Wilson of The Gap Band)"
--                     311
--                     "http://localhost:3000/goodfriday.m3u8"
--                 ]
--       }
--     , Cmd.none
--     )


load_data session =
    get_data session.sessionToken HandleData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleData res ->
            ( { model | data = res }
            , Cmd.none
            )

        LoadSong song ->
            ( { model | active = Just song }, Hls.initialize (E.string song.playlist) )

        Pause ->
            let
                _ =
                    Debug.log "here" model

                val =
                    "Hello"
            in
            ( model, Hls.pause (E.string val) )

        Play ->
            let
                _ =
                    Debug.log "here" model

                val =
                    "Hello"
            in
            ( model, Hls.play (E.string val) )

        _ ->
            ( model
            , Cmd.none
            )



-- View


sidebar =
    Element.column
        [ height fill
        , width (px 300)
        , Border.widthEach { edges | right = 1 }
        , Border.color (rgb255 227 227 227)
        , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 5, color = rgb255 217 217 217 }
        ]
        [ Styles.title "SALMON" [] ]


table_header =
    let
        header x =
            Element.el [ width <| fillPortion 1, Font.alignLeft ] <| text x
    in
    Element.row [ width fill, Border.widthEach { edges | bottom = 1 }, height (px 60) ]
        [ header "TITLE"
        , header "ARTIST"
        , header "ALBUM"
        , header "ADDED"
        , header "DURATION"
        ]


row_item value =
    Element.el [ clipX, width <| fillPortion 1, height (px 30) ] <| text value


song_row song =
    Element.row [ width fill, onClick <| LoadSong song ]
        [ row_item song.title
        , row_item song.artist
        , row_item song.album
        , row_item "5 Days Ago"
        , row_item (String.fromInt song.duration)
        ]


song_table songs =
    Element.column [ width fill, spacing 20 ] <| List.map song_row songs


button value handler =
    Input.button
        [ height (px 40)
        , centerX
        , Border.width 2
        , Border.color Styles.blue
        , Font.size 12
        , Font.center
        , Font.color Styles.blue
        , Font.bold
        , width (px 100)
        ]
        { onPress = Just handler
        , label = text value
        }


player title =
    Element.row [ alignBottom, Background.color (rgb255 207 152 153), width fill, height (px 50) ]
        [ html <| Html.audio [ HtmlAttribute.attribute "id" "hls-audio" ] []
        , el [] <| text title
        , button "Pause" Pause
        , button "Play" Play
        ]


render model =
    case model.data of
        Loading ->
            text "Loading Media Library..."

        Success songs ->
            -- HtmlAttribute.attribute "controls" ""
            let
                title =
                    case model.active of
                        Nothing ->
                            ""

                        Just x ->
                            "Now playing : " ++ x.title
            in
            Element.row
                [ width fill
                , height fill
                , inFront <| player title
                , Font.family [ Font.typeface "Open Sans" ]
                ]
                [ sidebar
                , Element.column
                    [ height fill
                    , width fill
                    , paddingXY 20 50
                    , spacing 10
                    ]
                    [ Styles.title "Songs" [ alignLeft ]
                    , table_header
                    , song_table songs
                    ]
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
