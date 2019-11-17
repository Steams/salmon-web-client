module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Components.Blank as Blank
import Components.Home as Home
import Components.NotFound as NotFound
import Components.Thing as Thing
import Element exposing (..)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Layout as Layout
import Route exposing (Route)
import Session exposing (Session)
import Task
import Url exposing (Url)


type Page
    = Redirect
    | Thing Thing.Model
    | Home Home.Model
    | NotFound


type alias Model =
    { session : Session, page : Page }



-- MODEL


init : Session.Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            Model (Session.init flags navKey) Redirect

        route =
            Route.fromUrl url
    in
    goto route model



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

        Thing thing ->
            render Layout.Thing ThingMsg (Thing.view thing)



-- UPDATE


type Msg
    = ChangeUrl Url
    | RequestUrl Browser.UrlRequest
    | HomeMsg Home.Msg
    | ThingMsg Thing.Msg
    | NoOp



-- | GotSession Session


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

        Just (Route.Thing x) ->
            let
                ( thing, thing_msg ) =
                    Thing.init model.session x
            in
            ( { model | page = Thing thing }
            , Cmd.map ThingMsg thing_msg
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

        ( ThingMsg subMsg, Thing x ) ->
            let
                ( thing_model, thing_msg ) =
                    Thing.update subMsg x
            in
            ( { model | page = Thing thing_model }
            , Cmd.map ThingMsg thing_msg
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
