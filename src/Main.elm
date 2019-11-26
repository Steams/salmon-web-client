module Main exposing (main)

import Api as Api
import Browser exposing (Document)
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Layout as Layout
import Pages.Blank as Blank
import Pages.Home as Home
import Pages.Login as Login
import Pages.NotFound as NotFound
import Pages.Signup as Signup
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Session exposing (Session)
import Task
import Url exposing (Url)
import Ports as Ports


type Page
    = Redirect
    | Home Home.Model
    | Login Login.Model
    | Signup Signup.Model
    | NotFound


type alias Model =
    { session : Session, page : Page }


init : Session.Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            Model (Session.init flags navKey) Redirect

        route =
            if model.session.sessionToken == "" then
                Just Route.Login
                -- Just Route.Home
                -- Route.fromUrl url

            else
                Route.fromUrl url
    in
    goto route model


type Msg
    = ChangeUrl Url
    | RequestUrl Browser.UrlRequest
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | VerificationResponse (WebData String)
    | NoOp


goto : Maybe Route -> Model -> ( Model, Cmd Msg )
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
            , Cmd.map HomeMsg home_msg
            )

        Just Route.Login ->
            let
                ( login, login_msg ) =
                    Login.init model.session
            in
            ( { model | page = Login login }
            , Cmd.map LoginMsg login_msg
            )

        Just Route.Signup ->
            let
                ( signup, signup_msg ) =
                    Signup.init model.session
            in
            ( { model | page = Signup signup }
            , Cmd.map SignupMsg signup_msg
            )

        Just (Route.Verification token) ->
            let
                _ =
                    Debug.log "verification request" token

                ( signup, signup_msg ) =
                    Signup.init model.session
            in
            ( { model | page = Redirect }
            , Api.verify VerificationResponse token
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( RequestUrl urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.session.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangeUrl url, _ ) ->
            goto (Route.fromUrl url) model

        ( VerificationResponse (Success token), _ ) ->
            let
                _ =
                    Debug.log "verification RESPONSE : " token

                session =
                    model.session

                new_session =
                    { session | sessionToken = token }
            in
            ( { model | session = new_session }
            , Nav.pushUrl session.navKey (Route.toUrl Route.Home)
            )

        ( VerificationResponse res, _ ) ->
            let
                _ =
                    Debug.log "Failed verificaiton response" res
            in
            ( model, Cmd.none )

        ( LoginMsg (Login.LoginResponse (Success id)), _ ) ->
            let
                _ =
                    Debug.log "ID RESPONSE : " id

                session =
                    model.session

                new_session =
                    { session | sessionToken = id }
            in
            ( { model | session = new_session }
            , Cmd.batch
                [ Ports.storeSession (Encode.string id)
                , Nav.pushUrl session.navKey (Route.toUrl Route.Home)
                ]
            )

        ( LoginMsg (Login.LoginResponse res), _ ) ->
            let
                _ =
                    Debug.log "Failed Login Response : " res
            in
            ( model, Cmd.none )

        ( LoginMsg subMsg, Login login ) ->
            let
                ( login_model, login_msg ) =
                    Login.update model.session subMsg login
            in
            ( { model | page = Login login_model }
            , Cmd.map LoginMsg login_msg
            )

        ( SignupMsg subMsg, Signup signup ) ->
            let
                ( signup_model, signup_msg ) =
                    Signup.update model.session subMsg signup
            in
            ( { model | page = Signup signup_model }
            , Cmd.map SignupMsg signup_msg
            )

        ( HomeMsg subMsg, Home home ) ->
            let
                ( home_model, home_msg ) =
                    Home.update subMsg home
            in
            ( { model | page = Home home_model }
            , Cmd.map HomeMsg home_msg
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    let
        render : Layout.Layout -> (subMsg -> Msg) -> Layout.TitleAndContent subMsg -> Document Msg
        render layout msg_wrapper page =
            Layout.render layout { title = page.title, content = Element.map msg_wrapper page.content }
    in
    case model.page of
        Redirect ->
            render Layout.Other (\_ -> NoOp) Blank.view

        NotFound ->
            render Layout.Other (\_ -> NoOp) NotFound.view

        Home home ->
            render Layout.Home HomeMsg (Home.view home)

        Login login ->
            render Layout.Login LoginMsg (Login.view login)

        Signup signup ->
            render Layout.Signup SignupMsg (Signup.view signup)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- MAIN


main : Program Session.Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangeUrl
        , onUrlRequest = RequestUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
