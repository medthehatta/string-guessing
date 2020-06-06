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
import Url exposing (Url)



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
        initialKey =
            key

        ( liftMsg, liftModel, subInit ) =
            ( GotHomeMsg, Home, Home.init )

        ( subModel, subCmdMsg ) =
            subInit flags
    in
    ( { key = key, page = liftModel subModel }, Cmd.map liftMsg subCmdMsg )



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

        ( UrlRequested urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( model, Cmd.none )

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



-- Lift pages into the main app


updateWith : (msg -> Msg) -> (model -> PageModel) -> Model -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
updateWith liftMsg liftModel model ( preModel, preMsg ) =
    ( { model | page = liftModel preModel }, Cmd.map liftMsg preMsg )


viewWith : (msg -> Msg) -> Html msg -> Browser.Document Msg
viewWith liftMsg subDoc =
    { title = "Strings", body = [ Html.map liftMsg subDoc ] }
