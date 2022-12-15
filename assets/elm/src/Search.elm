module Search exposing (InternalMsg, Model, Msg, cancelIcon, icon, init, searchForm, translator)

import Html exposing (Html, button, div, form, input)
import Html.Attributes exposing (class, placeholder, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr exposing (d, viewBox)


type alias Model =
    { term : String }


type Msg
    = Internal InternalMsg
    | External ExternalMsg


type InternalMsg
    = Input String


type ExternalMsg
    = Search String


init : Model
init =
    { term = ""
    }



----------------
-- TRANSLATOR --
----------------


type alias Translations msg =
    { onSearch : String -> msg
    , onInternal : InternalMsg -> msg
    }


type alias Translator msg =
    Msg -> msg


translator : Translations msg -> Translator msg
translator { onSearch, onInternal } msg =
    case msg of
        External (Search term) ->
            onSearch term

        Internal intMsg ->
            onInternal intMsg



-----------
-- VIEWS --
-----------


searchForm : Model -> Html Msg
searchForm model =
    form [ class "navigation_search-box", onSubmit <| External <| Search model.term ]
        [ searchInput model
        , button [ type_ "submit", class "navigation_search-box_go" ] [ goIcon ]
        , renderResults model
        ]


renderResults : Model -> Html Msg
renderResults _ =
    div [] []


goIcon : Html msg
goIcon =
    svg [ SvgAttr.class "svg-icon", viewBox "0 0 20 20" ]
        [ path [ d "M12.522,10.4l-3.559,3.562c-0.172,0.173-0.451,0.176-0.625,0c-0.173-0.173-0.173-0.451,0-0.624l3.248-3.25L8.161,6.662c-0.173-0.173-0.173-0.452,0-0.624c0.172-0.175,0.451-0.175,0.624,0l3.738,3.736C12.695,9.947,12.695,10.228,12.522,10.4 M18.406,10c0,4.644-3.764,8.406-8.406,8.406c-4.644,0-8.406-3.763-8.406-8.406S5.356,1.594,10,1.594C14.643,1.594,18.406,5.356,18.406,10M17.521,10c0-4.148-3.374-7.521-7.521-7.521c-4.148,0-7.521,3.374-7.521,7.521c0,4.147,3.374,7.521,7.521,7.521C14.147,17.521,17.521,14.147,17.521,10" ] [] ]


searchInput : Model -> Html Msg
searchInput model =
    input
        [ type_ "search"
        , value model.term
        , class "no-margin"
        , onInput (Internal << Input)
        , placeholder "Поиск…"
        , required True
        ]
        []


cancelIcon : Html msg
cancelIcon =
    svg [ SvgAttr.class "svg-icon toggled", viewBox "0 0 20 20" ]
        [ path [ d "M10.185,1.417c-4.741,0-8.583,3.842-8.583,8.583c0,4.74,3.842,8.582,8.583,8.582S18.768,14.74,18.768,10C18.768,5.259,14.926,1.417,10.185,1.417 M10.185,17.68c-4.235,0-7.679-3.445-7.679-7.68c0-4.235,3.444-7.679,7.679-7.679S17.864,5.765,17.864,10C17.864,14.234,14.42,17.68,10.185,17.68 M10.824,10l2.842-2.844c0.178-0.176,0.178-0.46,0-0.637c-0.177-0.178-0.461-0.178-0.637,0l-2.844,2.841L7.341,6.52c-0.176-0.178-0.46-0.178-0.637,0c-0.178,0.176-0.178,0.461,0,0.637L9.546,10l-2.841,2.844c-0.178,0.176-0.178,0.461,0,0.637c0.178,0.178,0.459,0.178,0.637,0l2.844-2.841l2.844,2.841c0.178,0.178,0.459,0.178,0.637,0c0.178-0.176,0.178-0.461,0-0.637L10.824,10z" ] [] ]


icon : Html msg
icon =
    svg [ SvgAttr.class "svg-icon", viewBox "0 0 20 20" ]
        [ path [ d "M18.125,15.804l-4.038-4.037c0.675-1.079,1.012-2.308,1.01-3.534C15.089,4.62,12.199,1.75,8.584,1.75C4.815,1.75,1.982,4.726,2,8.286c0.021,3.577,2.908,6.549,6.578,6.549c1.241,0,2.417-0.347,3.44-0.985l4.032,4.026c0.167,0.166,0.43,0.166,0.596,0l1.479-1.478C18.292,16.234,18.292,15.968,18.125,15.804 M8.578,13.99c-3.198,0-5.716-2.593-5.733-5.71c-0.017-3.084,2.438-5.686,5.74-5.686c3.197,0,5.625,2.493,5.64,5.624C14.242,11.548,11.621,13.99,8.578,13.99 M16.349,16.981l-3.637-3.635c0.131-0.11,0.721-0.695,0.876-0.884l3.642,3.639L16.349,16.981z" ] [] ]
