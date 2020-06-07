module Page.Home exposing (Model, Msg, init, update, view)

import Browser exposing (element)
import Browser.Navigation as Nav
import Color exposing (Color)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Http
import Json.Decode as D
import Routing exposing (Route(..))
import Url exposing (Url)



-- BASICS


main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- TYPES


type Model
    = DoRoute Route
    | Initial


initialModel =
    Initial


type Msg
    = Noop
    | RequestNewGame
    | GotNewGame (Result Http.Error String)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RequestNewGame, _ ) ->
            ( model, Cmd.none )

        ( GotNewGame result, _ ) ->
            case result of
                Ok id ->
                    ( DoRoute (GoGame id), Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNewGame model
        , viewDivider model
        , viewReplayGames model
        ]


viewNewGame model =
    button [ onClick RequestNewGame ] [ text "New Game" ]


viewDivider model =
    hr [] []


viewReplayGames model =
    div [] []



-- HTTP
-- Should probably move all API stuff to an API module


fetchNewGame : Cmd Msg
fetchNewGame =
    Http.post
        { url = "http://localhost:8008/api/games/"
        , expect = Http.expectJson GotNewGame newGameDecoder
        , body = Http.emptyBody
        }



-- String is for the GameId.  Share that type properly later


newGameDecoder : D.Decoder String
newGameDecoder =
    D.at [ "data", "subject" ] D.string
