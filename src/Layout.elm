module Layout exposing (Layout(..), TitleAndContent, render)

import Browser exposing (Document)
import Element exposing (..)
import Route exposing (Route)
import Styles


type Layout
    = Other
    | Home
    | Thing


type alias TitleAndContent msg =
    { title : String, content : Element msg }


render : Layout -> TitleAndContent msg -> Document msg
render layout page =
    let
        build content =
            Element.layoutWith
                { options =
                    [ focusStyle
                        { borderColor = Nothing
                        , backgroundColor = Nothing
                        , shadow = Nothing
                        }
                    ]
                }
                [ height fill, width fill ]
                (view_container layout content)
    in
    { title = page.title
    , body = [ build page.content ]
    }


view_container : Layout -> Element msg -> Element msg
view_container layout content =
    Element.column
        [ width fill ]
        [ header layout
        , content
        ]


header : Layout -> Element msg
header layout =
    Element.row Styles.nav_bar
        [ navLink layout Route.Home <| Element.el Styles.nav_item <| Element.text "Home"
        , navLink layout (Route.Thing "template") <| Element.el Styles.nav_item <| Element.text "Thing"
        ]


navLink layout route label =
    case isActive layout route of
        True ->
            Element.link Styles.active_nav_item { url = Route.toUrl route, label = label }

        False ->
            Element.link Styles.inactive_nav_item { url = Route.toUrl route, label = label }


isActive layout route =
    case ( layout, route ) of
        ( Home, Route.Home ) ->
            True

        ( Thing, Route.Thing s ) ->
            True

        _ ->
            False
