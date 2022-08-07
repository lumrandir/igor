module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html)
import Url exposing (Url)


type alias State =
    ()


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url


type Model
    = Model


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
init _ _ _ =
    ( Model, Cmd.none )


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
    []
