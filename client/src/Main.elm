module Main exposing (..)

import Browser exposing (application)
import Browser.Navigation as Nav
import Color exposing (Color)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Http
import Json.Decode as D
import Page.Game as Game
import Page.Home as Home
import Routing exposing (Route(..))
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser)



-- BASICS


main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- TYPES


type alias Model =
    { key : Nav.Key
    , page : PageModel
    }


type PageModel
    = Game Game.Model
    | Home Home.Model
    | Loading


type Msg
    = Noop
    | Trash -- This is just here so our update patterns are not exhaustive
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GotGameMsg Game.Msg
    | GotHomeMsg Home.Msg



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            { key = key, page = Loading }

        initialMsg =
            Nav.pushUrl key (Url.toString url)
    in
    ( initialModel, initialMsg )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( GotGameMsg subMsg, Game subModel ) ->
            let
                subUpdate =
                    Game.update subMsg subModel
            in
            updateWith GotGameMsg Game model subUpdate

        ( Route subRoute, _ ) ->
            case subRoute of
                GoHome ->
                    Home.init () |> updateWith GotHomeMsg Home model

                GoGame id ->
                    Game.init id |> updateWith GotGameMsg Game model

        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            let
                maybeGameId =
                    P.parse (P.s "game" </> P.string) url
            in
            case maybeGameId of
                Nothing ->
                    Home.init () |> updateWith GotHomeMsg Home model

                Just id ->
                    Game.init id
                        |> updateWith GotGameMsg Game model

        -- Ignore all other messages
        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Game subModel ->
            Game.view subModel |> viewWith GotGameMsg

        Home subModel ->
            Home.view subModel |> viewWith GotHomeMsg

        Loading ->
            { title = "Loading"
            , body = [ div [] [ h1 [] [ text "Loading..." ] ] ]
            }



-- Lift pages into the main app


updateWith : (msg -> Msg) -> (model -> PageModel) -> Model -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
updateWith liftMsg liftModel model ( preModel, preMsg ) =
    ( { model | page = liftModel preModel }, Cmd.map liftMsg preMsg )


viewWith : (msg -> Msg) -> Html msg -> Browser.Document Msg
viewWith liftMsg subDoc =
    { title = "Strings", body = [ Html.map liftMsg subDoc ] }
