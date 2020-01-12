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
import Pages.Instructions as Instructions
import Pages.Landing as Landing
import Pages.Login as Login
import Pages.NotFound as NotFound
import Pages.Signup as Signup
import Player as Player
import Ports as Ports
import RemoteData as RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Session exposing (Session)
import Task
import Url exposing (Url)


type Page
    = Redirect
    | Player Player.Model
    | Login Login.Model
    | Signup Signup.Model
    | Landing Landing.Model
    | Instructions Instructions.Model
    | NotFound


type alias Model =
    { session : Session, page : Page }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            Model (Session.init navKey) Redirect

        -- TODO REMOVE BELOW FOR REAL TESTS
        -- Model { csrfToken = "test", navKey = navKey } Redirect
        route =
            -- if model.session.sessionToken == "" then
            --     Just Route.Login
            --     -- Just Route.Landing
            --     -- Route.fromUrl url
            -- else
            Route.fromUrl url
    in
    goto route model


type Msg
    = ChangeUrl Url
    | RequestUrl Browser.UrlRequest
    | PlayerMsg Player.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | LandingMsg Landing.Msg
    | InstructionsMsg Instructions.Msg
    | VerificationResponse (WebData String)
    | CsrfResponse Route (WebData String)
    | NoOp


goto : Maybe Route -> Model -> ( Model, Cmd Msg )
goto maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Root ->
            ( model, Route.replaceUrl model.session.navKey Route.Landing )

        Just Route.Player ->
            case model.session.csrfToken of
                "" ->
                    ( model, get_csrf Route.Player )

                _ ->
                    let
                        ( player, player_msg ) =
                            Player.init model.session
                    in
                    ( { model | page = Player player }, Cmd.map PlayerMsg player_msg )

        Just Route.Landing ->
            let
                ( landing, landing_msg ) =
                    Landing.init model.session
            in
            ( { model | page = Landing landing }
            , Cmd.map LandingMsg landing_msg
            )

        Just Route.Instructions ->
            let
                ( instructions, instructions_msg ) =
                    Instructions.init model.session
            in
            ( { model | page = Instructions instructions }
            , Cmd.map InstructionsMsg instructions_msg
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


get_csrf : Route -> Cmd Msg
get_csrf destination =
    Api.get_csrf <| CsrfResponse destination


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

        ( CsrfResponse route (Success token), _ ) ->
            let
                session =
                    model.session

                new_session =
                    { session | csrfToken = token }
            in
            goto (Just route) { model | session = new_session }

        ( CsrfResponse route res, _ ) ->
            goto (Just Route.Login) model

        ( VerificationResponse (Success token), _ ) ->
            let
                _ =
                    Debug.log "verification RESPONSE : " token

                session =
                    model.session

                new_session =
                    { session | csrfToken = token }
            in
            ( { model | session = new_session }
            , Nav.pushUrl session.navKey (Route.toUrl Route.Player)
            )

        ( VerificationResponse res, _ ) ->
            let
                _ =
                    Debug.log "Failed verificaiton response" res
            in
            ( model, Cmd.none )

        ( LoginMsg (Login.LoginResponse (Success csrf)), _ ) ->
            let
                session =
                    model.session

                new_session =
                    { session | csrfToken = csrf }
            in
            ( { model | session = new_session }
            , Cmd.batch
                [ Nav.pushUrl session.navKey (Route.toUrl Route.Player)
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

        ( LandingMsg subMsg, Landing landing ) ->
            let
                ( landing_model, landing_msg ) =
                    Landing.update model.session subMsg landing
            in
            ( { model | page = Landing landing_model }
            , Cmd.map LandingMsg landing_msg
            )

        ( InstructionsMsg subMsg, Instructions instructions ) ->
            let
                ( instructions_model, instructions_msg ) =
                    Instructions.update model.session subMsg instructions
            in
            ( { model | page = Instructions instructions_model }
            , Cmd.map InstructionsMsg instructions_msg
            )

        ( PlayerMsg subMsg, Player player ) ->
            let
                ( player_model, player_msg ) =
                    Player.update subMsg player
            in
            ( { model | page = Player player_model }
            , Cmd.map PlayerMsg player_msg
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

        Player player ->
            render Layout.Player PlayerMsg (Player.view player)

        Login login ->
            render Layout.Login LoginMsg (Login.view login)

        Signup signup ->
            render Layout.Signup SignupMsg (Signup.view signup)

        Landing landing ->
            render Layout.Landing LandingMsg (Landing.view landing)

        Instructions instructions ->
            render Layout.Instructions InstructionsMsg (Instructions.view instructions)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Player player ->
            Sub.batch
                [ Sub.map PlayerMsg (Player.subscriptions player)
                ]

        Login login ->
            Sub.batch
                [ Sub.map LoginMsg (Login.subscriptions login)
                ]

        Signup signup ->
            Sub.batch
                [ Sub.map SignupMsg (Signup.subscriptions signup)
                ]

        _ ->
            Sub.none



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
