module Main exposing (main)

import App as App
import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Layout as Layout
import Pages.Blank as Blank
import Pages.Home as Home
import Pages.Login as Login
import Pages.NotFound as NotFound
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Session exposing (Session)
import Task
import Url exposing (Url)


type Page
    = Redirect
    | Home Home.Model
    | Login Login.Model
    | NotFound


type alias Model =
    { session : Session, page : Page }



-- MODEL


init : Session.Flags -> Url -> Nav.Key -> ( Model, Cmd (App.Msg a) )
init flags url navKey =
    let
        model =
            Model (Session.init flags navKey) Redirect

        route =
            Route.fromUrl url
    in
    goto route model



-- VIEW


view : Model -> Document (App.Msg msg)
view model =
    let
        -- render : Layout.Layout -> (subMsg -> (App.Msg msg)) -> Layout.TitleAndContent subMsg -> Document Msg
        -- render layout msg_wrapper page =
        --     Layout.render layout { title = page.title, content = Element.map msg_wrapper page.content }
        -- render layout page =
        --     Layout.render layout { title = page.title, content = page.content }
        _ =
            ""
    in
    case model.page of
        Redirect ->
            Layout.render Layout.Other Blank.view

        NotFound ->
            Layout.render Layout.Other NotFound.view

        Home home ->
            Layout.render Layout.Home (Home.view home)

        Login login ->
            Layout.render Layout.Login (Login.view login)



-- Redirect ->
--     render Layout.Other (\_ -> NoOp) Blank.view
-- NotFound ->
--     render Layout.Other (\_ -> NoOp) NotFound.view
-- Home home ->
--     render Layout.Home (App.HomeMsg >> App.LocalMsg) (Home.view home)
-- Login login ->
--     render Layout.Login LoginMsg (Login.view login)
-- UPDATE
-- type Msg
--     = ChangeUrl Url
--     | RequestUrl Browser.UrlRequest
--     | HomeMsg Home.Msg
--     | LoginMsg Login.Msg
--     | GlobalMsg App.Msg
--     | NoOp


goto : Maybe Route -> Model -> ( Model, Cmd (App.Msg a) )
goto maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl model.session.navKey Route.Home )

        Just Route.Home ->
            let
                ( home, home_msg ) =
                    Home.init model.session
            in
            ( { model | page = Home home }
            , home_msg
            )

        Just Route.Login ->
            let
                ( login, login_msg ) =
                    Login.init model.session
            in
            ( { model | page = Login login }
            , login_msg
            )



-- LoginResponse (Success id) ->
--     let _= Debug.log "ID RESPONSE : " id
--     in
--     ( model
--     ,
--     )
-- LoginResponse data ->
--     let _= Debug.log "Login failed RESPONSE : " data
--     in
--     ( model
--     , Cmd.none
--     )

handle_global_msg : App.GlobalMsg -> Model -> ( Model, Cmd (App.Msg a) )
handle_global_msg msg model =
    case msg of
        App.RequestUrl urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.session.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        App.ChangeUrl url ->
            goto (Route.fromUrl url) model

        App.LoginResponse (Success id) ->
            let
                session =
                    model.session

                new_session =
                    { session | sessionToken = id }
            in
            ( { model | session = new_session }
            , Cmd.none
            )

        App.LoginResponse res ->
            let
                _ =
                    Debug.log "login response :" res
            in
            ( model, Cmd.none )


handle_local_msg : App.PageMsg a -> Model -> ( Model, Cmd (App.Msg a) )
handle_local_msg msg model =
    case ( msg, model.page ) of
        ( App.LoginMsg subMsg, Login login ) ->
            let
                ( login_model, login_msg ) =
                    Login.update subMsg login
            in
            ( { model | page = Login login_model }
            , login_msg
            )

        ( App.HomeMsg subMsg, Home home ) ->
            let
                ( home_model, home_msg ) =
                    Home.update subMsg home
            in
            ( { model | page = Home home_model }
            , home_msg
            )

        ( _, _ ) ->
            ( model, Cmd.none )

update : (App.Msg a) -> Model -> ( Model, Cmd (App.Msg a) )
update msg model =
    case msg of
        App.Global g ->
            handle_global_msg g model

        App.LocalMsg l ->
            handle_local_msg l model






-- SUBSCRIPTIONS


subscriptions : Model -> Sub (App.Msg a)
subscriptions model =
    Sub.batch
        []



-- MAIN


main : Program Session.Flags Model (App.Msg a)
main =
    Browser.application
        { init = init
        , onUrlChange = App.ChangeUrl
        , onUrlRequest = App.RequestUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
