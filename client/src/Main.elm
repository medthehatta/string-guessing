module Main exposing (..)

import Browser exposing (element)
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attrs
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Http
import Json.Decode as D



-- BASICS


main =
    element { init = init, view = view, update = update, subscriptions = subscriptions }


quickSamples num =
    let
        wrap_ txt =
            { text = txt, id = txt }
    in
    List.map wrap_ (List.map (\x -> "S" ++ String.fromInt x) (List.range 1 num))


quickContracts num =
    let
        wrap_ txt =
            { text = txt, id = txt }
    in
    List.map wrap_ (List.map (\x -> "C" ++ String.fromInt x) (List.range 1 num))


quickTests num =
    let
        wrap_ txt =
            { text = txt, id = txt }
    in
    List.map wrap_ (List.map (\x -> "T" ++ String.fromInt x) (List.range 1 num))


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, fetchConstantGame )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- TYPES


type alias Contract =
    { text : String, id : String }


type alias Test =
    { text : String, id : String }


type alias Sample =
    { text : String, id : String }


type alias TestResult =
    String


type alias GameStateReply =
    { tests : List Test
    , samples : List Sample
    , measures : Dict String (Dict String Int)
    , answers : Dict String String
    }


type alias Model =
    { contracts : List Contract
    , tests : List Test
    , samples : List Sample
    , results : List ( Sample, Test, TestResult )
    , measures : Dict String (Dict String Int)
    , answers : Dict String String
    , answersRevealed : Bool
    , money : Int
    , selectedSample : Maybe Sample
    }


initialModel =
    { contracts = []
    , tests = []
    , samples = []
    , results = []
    , measures = Dict.fromList []
    , answers = Dict.fromList []
    , answersRevealed = False
    , money = 0
    , selectedSample = Nothing
    }


type Msg
    = Noop
    | SampleClicked Sample
    | TestClicked Test
    | GotGameState (Result Http.Error GameStateReply)
    | ShowAnswers
    | HideAnswers



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SampleClicked sample ->
            case model.selectedSample of
                Nothing ->
                    ( { model | selectedSample = Just sample }, Cmd.none )

                Just sample_ ->
                    if sample_ == sample then
                        ( { model | selectedSample = Nothing }, Cmd.none )

                    else
                        ( { model | selectedSample = Just sample }, Cmd.none )

        TestClicked test ->
            case model.selectedSample of
                Nothing ->
                    ( model, Cmd.none )

                Just sample ->
                    let
                        newModel =
                            let
                                result =
                                    Dict.get sample.id model.measures
                                        |> Maybe.andThen (Dict.get test.id)
                                        |> Maybe.withDefault 0
                                        |> String.fromInt
                            in
                            { model | selectedSample = Nothing, results = model.results ++ [ ( sample, test, result ) ] }
                    in
                    ( newModel, Cmd.none )

        GotGameState s ->
            case s of
                Ok { tests, samples, measures, answers } ->
                    ( { model | tests = tests, samples = samples, measures = measures, answers = answers }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        ShowAnswers ->
            ( { model | answersRevealed = True }, Cmd.none )

        HideAnswers ->
            ( { model | answersRevealed = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        [ viewSamples model.samples model.selectedSample
        , viewTests model.tests
        , viewResults model.results
        ]
            ++ (if model.answersRevealed == True then
                    [ viewAnswers model.answers ]

                else
                    []
               )


sectionAttrs =
    [ Attrs.style "float" "left", Attrs.style "width" "100%" ]


viewSamples samples selectedSample =
    div sectionAttrs
        [ h1 [] [ text "Samples" ]
        , viewButtons
            { caption = \x -> x.text
            , signal = \x -> SampleClicked x
            , style =
                \x ->
                    case selectedSample of
                        Nothing ->
                            defaultButtonStyle

                        Just selectedSample_ ->
                            if x.id == selectedSample_.id then
                                selectedButtonStyle

                            else
                                defaultButtonStyle
            }
            samples
        ]


viewTests tests =
    div sectionAttrs
        [ h1 [] [ text "Tests" ]
        , viewButtons
            { caption = \x -> x.text
            , signal = \x -> TestClicked x
            , style = \_ -> defaultButtonStyle
            }
            tests
        ]


viewResults results =
    let
        viewResult ( sample, test, r ) =
            td [ Attrs.style "font-size" "24px" ] [ text (sample.text ++ " " ++ test.text ++ " = " ++ r) ]
    in
    div sectionAttrs
        [ h1 [] [ text "Results" ]
        , table []
            (List.map
                (\res ->
                    tr [] [ viewResult res ]
                )
                results
            )
        , a [ Attrs.href "#", onClick ShowAnswers ] [ text "(reveal answers)" ]
        ]


viewAnswers answers =
    div sectionAttrs
        [ h1 [] [ text "Answers" ]
        , table []
            (List.map
                (\( name, answer ) ->
                    tr [] [ td [] [ text name ], td [] [ text answer ] ]
                )
                (Dict.toList answers)
            )
        , a [ Attrs.href "#", onClick HideAnswers ] [ text "(hide answers)" ]
        ]


defaultButtonStyle : List (Attribute Msg)
defaultButtonStyle =
    let
        styles =
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            , ( "width", "150px" )
            , ( "height", "150px" )
            , ( "line-height", "30px" )
            , ( "float", "left" )
            , ( "background", "lightgray" )
            , ( "margin", "2px" )
            , ( "text-align", "center" )
            , ( "text-decoration", "none" )
            , ( "font-weight", "bold" )
            , ( "font-family", "sans" )
            , ( "font-size", "24px" )
            ]
    in
    List.map (\( x, y ) -> Attrs.style x y) styles


selectedButtonStyle =
    defaultButtonStyle ++ [ Attrs.style "border" "4px solid black" ]


viewButtons : { caption : a -> String, signal : a -> Msg, style : a -> List (Attribute Msg) } -> List a -> Html Msg
viewButtons { caption, signal, style } buttons =
    div [ Attrs.style "float" "left" ]
        (List.map
            (\x -> viewButton (caption x) (style x) (signal x))
            buttons
        )


viewButton : String -> List (Attribute Msg) -> Msg -> Html Msg
viewButton txt buttonStyle signal =
    a (buttonStyle ++ [ onClick signal ]) [ text txt ]



-- HTTP


fetchConstantGame : Cmd Msg
fetchConstantGame =
    Http.get
        { url = "game.json"
        , expect = Http.expectJson GotGameState constantGameDecoder
        }


constantGameDecoder : D.Decoder GameStateReply
constantGameDecoder =
    let
        nameToTest : D.Decoder Test
        nameToTest =
            D.map2
                (\x -> \y -> { text = x, id = y })
                D.string
                D.string

        nameToSample : D.Decoder Sample
        nameToSample =
            D.map2
                (\x -> \y -> { text = x, id = y })
                D.string
                D.string
    in
    D.map4
        (\x -> \y -> \z -> \w -> { tests = x, samples = y, measures = z, answers = w })
        (D.at [ "tests" ] (D.list nameToTest))
        (D.at [ "samples" ] (D.list nameToSample))
        (D.at [ "measures" ] (D.dict (D.dict D.int)))
        (D.at [ "answers" ] (D.dict D.string))
