module Main exposing (..)

import Browser exposing (element)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Http
import Json.Decode as D


main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( Loading, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Model
    = Loading


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ h1 [] [ text "Working." ] ]
