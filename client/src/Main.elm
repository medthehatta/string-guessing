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
    , resultsExpanded : Bool
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
    , resultsExpanded = True
    }


type Msg
    = Noop
    | SampleClicked Sample
    | TestClicked Test
    | GotGameState (Result Http.Error GameStateReply)
    | ShowAnswers
    | HideAnswers
    | ToggleResults



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

        ToggleResults ->
            case model.resultsExpanded of
                True ->
                    ( { model | resultsExpanded = False }, Cmd.none )

                False ->
                    ( { model | resultsExpanded = True }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        showp =
            if model.answersRevealed == True then
                "hide"

            else
                "reveal"

        showm =
            if model.answersRevealed == True then
                HideAnswers

            else
                ShowAnswers
    in
    div [] <|
        [ viewSamples model.samples model.selectedSample
        , viewTests model.tests
        , viewResults model.results model.resultsExpanded
        , a [ Attrs.class "outer", Attrs.href "#", onClick showm ] [ text <| "(" ++ showp ++ " answers)" ]
        ]
            ++ (if model.answersRevealed == True then
                    [ viewAnswers model.answers ]

                else
                    []
               )
            ++ [ viewInstructions ]


sectionAttrs =
    [ Attrs.class "section" ]


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


viewResults results expanded =
    let
        viewResult ( sample, test, r ) =
            li [ Attrs.class "outer" ] [ text (sample.text ++ " " ++ test.text ++ " = " ++ r) ]

        exStyle =
            if expanded == True then
                { arrowClass = "fas fa-angle-down"
                , arrowAlt = "Collapse"
                , disp = Attrs.style "display" "block"
                }

            else
                { arrowClass = "fas fa-angle-up"
                , arrowAlt = "Expand"
                , disp = Attrs.style "display" "none"
                }
    in
    div [ Attrs.class "footer" ]
        [ h1 [ onClick ToggleResults ] [ i [ Attrs.class exStyle.arrowClass, Attrs.alt exStyle.arrowAlt ] [], span [] [ text "Results" ] ]
        , ol [ exStyle.disp ]
            (List.map viewResult results)
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
        ]


viewInstructions =
    div sectionAttrs
        [ hr [] []
        , h1 [] [ text "Instructions" ]
        , div []
            [ h2 [] [ text "Goal" ]
            , p [] [ text "Guess the composition of the 'samples', which are 5-letter strings made of A, B, C, and D" ]
            ]
        , div []
            [ h2 [] [ text "Samples" ]
            , p [] [ text "The samples are each secret 5-letter strings made of A, B, C, and D.  The samples are named randomly; the name has no relation to their underlying composition!" ]
            ]
        , div []
            [ h2 [] [ text "Tests" ]
            , ul []
                [ li [] [ text "A - Number of times A appears at any position in the sample" ]
                , li [] [ text "AA - Number of times A appears in a set of TWO OR MORE anywhere in the sample" ]
                , li [] [ text "AB - Number of times AB appears in that order anywhere in the sample" ]
                , li [] [ text ".AAA. - Number of A's present in the central 3 positions of the string" ]
                ]
            ]
        , div []
            [ h2 [] [ text "How to play" ]
            , p [] [ text "Click the sample you want to test, then the test you want to perform on it.  The answer will appear in the 'Results' list" ]
            , p [] [ text "When you're ready to check your work, click the 'reveal answers' link" ]
            , div [ Attrs.class "footer-space" ] []
            ]
        ]


defaultButtonStyle : List (Attribute Msg)
defaultButtonStyle =
    [ Attrs.class "button" ]


selectedButtonStyle =
    defaultButtonStyle ++ [ Attrs.class "bordered" ]


viewButtons : { caption : a -> String, signal : a -> Msg, style : a -> List (Attribute Msg) } -> List a -> Html Msg
viewButtons { caption, signal, style } buttons =
    div [ Attrs.class "section" ]
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
