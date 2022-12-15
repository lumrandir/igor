module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, input, label, main_, nav, ul)
import Html.Attributes exposing (class, for, id, type_)
import Html.Events exposing (onClick)
import Search
import Svg exposing (rect, svg)
import Svg.Attributes as SvgAttr
import Url exposing (Url)


type alias State =
    ()


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | Search String
    | ToggleMenu
    | SearchMsg Search.InternalMsg


type alias Model =
    { key : Nav.Key
    , search : Search.Model
    }


main : Program State Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : State -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , search = Search.init
      }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    { title = "Igor"
    , body = layout model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []


layout : Model -> List (Html Msg)
layout model =
    [ navigation model
    , main_ [ class "container wrapper" ]
        (render model)
    ]


render : Model -> List (Html Msg)
render _ =
    []



-----------------
-- TRANSLATORS --
-----------------


searchTranslations =
    { onSearch = Search
    , onInternal = SearchMsg
    }


searchTranslator : Search.Msg -> Msg
searchTranslator =
    Search.translator searchTranslations



-----------
-- VIEWS --
-----------


navigation : Model -> Html Msg
navigation model =
    div [ class "navigation" ]
        [ nav []
            [ input [ type_ "checkbox", id "show-search" ] []
            , div [ class "navigation_content" ]
                [ div [ class "navigation_content_logo" ] []
                , div [ class "navigation_content_menu" ]
                    [ menuIcon ]
                , ul [ class "navigation_content_links", id "menu" ] []
                ]
            , label [ for "show-search", class "navigation_search-icon" ] [ Search.icon, Search.cancelIcon ]
            , Search.searchForm model.search |> Html.map searchTranslator
            ]
        ]


menuIcon : Html Msg
menuIcon =
    svg [ SvgAttr.viewBox "0 0 100 80", SvgAttr.width "40", SvgAttr.height "40", onClick ToggleMenu ]
        [ rect
            [ SvgAttr.width "100"
            , SvgAttr.height "15"
            , SvgAttr.fill "#606c76"
            ]
            []
        , rect
            [ SvgAttr.y "25"
            , SvgAttr.width "100"
            , SvgAttr.height "15"
            , SvgAttr.fill "#606c76"
            ]
            []
        , rect
            [ SvgAttr.y "50"
            , SvgAttr.width "100"
            , SvgAttr.height "15"
            , SvgAttr.fill "#606c76"
            ]
            []
        ]
