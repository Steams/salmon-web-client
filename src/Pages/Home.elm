module Pages.Home exposing (Model, Msg, init, update, view)

import App as App
import Api exposing (Song, get_data)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Events exposing (onClick)
import Hls exposing (initialize)
import Html as Html
import Html.Attributes as HtmlAttribute
import Json.Encode as E
import Layout exposing (TitleAndContent)
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Session exposing (Session)
import Styles



-- Model


type alias Model =
    { data : WebData (List Song) }



-- State
type Msg
    = NoOp
    | HandleData (WebData (List Song))
    | LoadSong String


init : Session -> ( Model, Cmd (App.Msg msg) )
init session =
    ( { data = Loading }, load_data session )


to_app_msg x = x >> App.HomeMsg >> App.LocalMsg

load_data session =
    get_data session.sessionToken <| to_app_msg HandleData


update : Msg -> Model -> ( Model, Cmd (App.Msg msg) )
update msg model =
    case msg of
        HandleData res ->
            ( { model | data = res }
            , Cmd.none
            )

        LoadSong url ->
            ( model, Hls.initialize (E.string url) )

        _ ->
            ( model
            , Cmd.none
            )



-- View


display_song song =
    Element.column [ onClick <| (to_app_msg LoadSong) song.playlist ]
        [ text <| "Title : " ++ song.title
        , text <| "Duration : " ++ String.fromInt song.duration
        , text <| "URL : " ++ song.playlist
        ]


render model =
    case model.data of
        Loading ->
            text "Loading Media Library..."

        Success songs ->
            let
                song_views =
                    List.map display_song songs

                player =
                    html <|
                        Html.audio
                            [ HtmlAttribute.attribute "controls" ""
                            , HtmlAttribute.attribute "id" "hls-audio"
                            ]
                            []
            in
            Element.column [] <| song_views ++ [ player ]

        Failure error ->
            let
                _ =
                    Debug.log "error" error
            in
            text "Failed to load Media Library"

        NotAsked ->
            text "Not Asked"


view : Model -> TitleAndContent (App.Msg msg)
view model =
    { title = "Contacts"
    , content = render model
    }
