module Main exposing (..)

import Browser exposing (element)
import Color exposing (Color)
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, fetchConstantGame )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- TYPES


type alias Contract =
    String


type alias Test =
    String


type alias Sample =
    String


type alias TestResult =
    String


type alias GameStateReply =
    { measures : Dict String (Dict String Int)
    , contracts : Dict String (Dict String Bool)
    , answers : Dict String String
    }


type alias Model =
    { tests : List Test
    , samples : List Sample
    , sampleColoring : Dict Sample Color
    , results : List ( Sample, Test, TestResult )
    , measures : Dict String (Dict String Int)
    , answers : Dict String String
    , contracts : Dict String (Dict String Bool)
    , answersRevealed : Bool
    , money : Int
    , selectedSample : Maybe Sample
    , resultsExpanded : Bool
    }


initialModel =
    { tests = []
    , samples = []
    , sampleColoring = Dict.empty
    , results = []
    , measures = Dict.empty
    , answers = Dict.empty
    , contracts = Dict.empty
    , answersRevealed = False
    , money = 100
    , selectedSample = Nothing
    , resultsExpanded = True
    }


type Msg
    = Noop
    | GotGameState (Result Http.Error GameStateReply)
    | SampleClicked Sample
    | TestClicked Test
    | ContractClicked String
    | ToggleAnswers
    | ToggleResults



-- UPDATE


pickOrToggle : Maybe a -> a -> Maybe a
pickOrToggle match value =
    case match of
        Nothing ->
            Just value

        Just match_ ->
            if match_ == value then
                Nothing

            else
                Just value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SampleClicked sample ->
            ( { model | selectedSample = pickOrToggle model.selectedSample sample }, Cmd.none )

        TestClicked test ->
            case model.selectedSample of
                Nothing ->
                    ( model, Cmd.none )

                Just sample ->
                    let
                        result =
                            Dict.get sample model.measures
                                |> Maybe.andThen (Dict.get test)
                                |> Maybe.withDefault 0
                                |> String.fromInt

                        newModel =
                            { model
                                | selectedSample = Nothing
                                , results = model.results ++ [ ( sample, test, result ) ]
                            }
                    in
                    ( newModel, Cmd.none )

        ContractClicked contractString ->
            case model.selectedSample of
                Nothing ->
                    ( model, Cmd.none )

                Just sample ->
                    let
                        result =
                            Dict.get sample model.contracts
                                |> Maybe.andThen (Dict.get contractString)
                                |> Maybe.withDefault False

                        newModel =
                            { model | selectedSample = Nothing }
                    in
                    ( newModel, Cmd.none )

        GotGameState s ->
            case s of
                Ok { measures, answers, contracts } ->
                    let
                        okColors =
                            [ Color.red
                            , Color.brown
                            , Color.green
                            , Color.blue
                            , Color.purple
                            , Color.orange
                            ]

                        samples : List Sample
                        samples =
                            Dict.keys measures

                        tests : List Test
                        tests =
                            Dict.values measures
                                |> List.head
                                |> Maybe.withDefault Dict.empty
                                |> Dict.keys

                        sampleColoring : Dict Sample Color
                        sampleColoring =
                            Dict.fromList <|
                                List.map2
                                    Tuple.pair
                                    samples
                                    okColors

                        newModel =
                            { model
                                | tests = tests
                                , samples = samples
                                , measures = measures
                                , answers = answers
                                , contracts = contracts
                                , sampleColoring = sampleColoring
                            }
                    in
                    ( newModel, Cmd.none )

                Err e ->
                    -- Do nothing
                    ( model, Cmd.none )

        ToggleAnswers ->
            ( { model | answersRevealed = not model.answersRevealed }, Cmd.none )

        ToggleResults ->
            ( { model | resultsExpanded = not model.resultsExpanded }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        showp =
            if model.answersRevealed == True then
                "hide"

            else
                "reveal"
    in
    div [] <|
        [ viewSamples model.samples model.selectedSample model.sampleColoring
        , viewTests model.tests
        , viewResults model.results model.resultsExpanded model.sampleColoring
        , viewContracts model.contracts
        , a [ Attrs.class "outer", onClick ToggleAnswers ] [ text <| "(" ++ showp ++ " answers)" ]
        ]
            ++ (if model.answersRevealed == True then
                    [ viewAnswers model.answers ]

                else
                    []
               )
            ++ [ viewInstructions ]


sectionAttrs =
    [ Attrs.class "section" ]


viewSamples samples selectedSample sampleColoring =
    let
        getColor sample =
            Dict.get sample sampleColoring |> Maybe.withDefault Color.black
    in
    div sectionAttrs
        [ h1 [] [ text "Samples" ]
        , viewButtons
            { caption = \x -> x
            , signal = \x -> SampleClicked x
            , style =
                \x ->
                    case selectedSample of
                        Nothing ->
                            defaultButtonStyle ++ [ Attrs.style "background" (Color.toCssString <| getColor x) ]

                        Just selectedSample_ ->
                            if x == selectedSample_ then
                                selectedButtonStyle ++ [ Attrs.style "background" (Color.toCssString <| getColor x) ]

                            else
                                defaultButtonStyle ++ [ Attrs.style "background" (Color.toCssString <| getColor x) ]
            }
            samples
        ]


viewTests tests =
    div sectionAttrs
        [ h1 [] [ text "Tests" ]
        , viewButtons
            { caption = \x -> x
            , signal = \x -> TestClicked x
            , style = \_ -> defaultButtonStyle
            }
            tests
        ]


viewContracts contracts =
    let
        contractNamesAll =
            List.map Dict.keys (Dict.values contracts)

        contractNames =
            List.head contractNamesAll |> Maybe.withDefault []
    in
    div sectionAttrs
        [ h1 [] [ text "Contracts" ]
        , viewButtons
            { caption = \x -> x
            , signal = \x -> ContractClicked x
            , style = \_ -> defaultButtonStyle
            }
            contractNames
        ]


viewResults results expanded sampleColoring =
    let
        getColor sample =
            Dict.get sample sampleColoring |> Maybe.withDefault Color.black

        resultStyle sample =
            [ Attrs.class "outer", Attrs.style "color" (Color.toCssString <| getColor sample) ]

        viewResult : ( Sample, Test, TestResult ) -> Html Msg
        viewResult ( sample, test, result ) =
            li (resultStyle sample) [ text (sample ++ " " ++ test ++ " = " ++ result) ]

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
        , table [ Attrs.class "answer-table" ]
            (List.map
                (\( name, answer ) ->
                    tr [] [ td [] [ span [] [ text name ], span [] [ text answer ] ] ]
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
            , p [] [ text "The samples are each secret 5-letter strings made of A, B, C, and D." ]
            ]
        , div []
            [ h2 [] [ text "Tests" ]
            , ul []
                [ li [] [ b [] [ text "A - " ], text "Number of times A appears at any position in the sample.  (E.G. C = 2 means there are 2 C's in the sample)" ]
                , li [] [ b [] [ text "AA - " ], text "Number of times A appears twice in a row.  (E.G. AA = 1 for 'AABBB', AA = 2 for 'AAABB')" ]
                , li [] [ b [] [ text "AB - " ], text "Number of times AB appears in that order anywhere in the sample  (E.G. CD is in the samples 'ACDCB' and 'CDDDD', but not 'DCBBB' or 'CCCBD')" ]
                , li [] [ b [] [ text "AAA.. - " ], text "Number of A's present in the first 3 positions of the string" ]
                , li [] [ b [] [ text ".AAA. - " ], text "Number of A's present in the central 3 positions of the string" ]
                , li [] [ b [] [ text "..AAA - " ], text "Number of A's present in the final 3 positions of the string" ]
                , li [] [ b [] [ text "A.A.A - " ], text "Number of A's present in the 3 string positions indicated" ]
                , li [] [ text "(Other tests with periods are similar: periods represent 'anything', and the letters mark places where the letter is being counted)" ]
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
    D.map3
        (\x y z -> { measures = x, answers = y, contracts = z })
        (D.at [ "measures" ] (D.dict (D.dict D.int)))
        (D.at [ "answers" ] (D.dict D.string))
        (D.at [ "contracts" ] (D.dict (D.dict D.bool)))
