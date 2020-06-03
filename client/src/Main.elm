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
import Url exposing (Url)



-- BASICS


main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( initialModel, fetchConstantGame )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ =
    Noop


onUrlChange : Url -> Msg
onUrlChange _ =
    Noop



-- TYPES


type alias Contract =
    String


type alias Test =
    String


type alias Sample =
    String


type Answer
    = Answer Sample String


type SampleResult
    = TestResult Sample Test Int
    | ContractResult Sample Contract Bool


type alias GameSetup =
    { measures : Dict String (Dict String Int)
    , contracts : Dict String (Dict String Bool)
    , answers : Dict String String
    }


type alias SetupInterrogator =
    { measure : Sample -> Test -> SampleResult
    , tryContract : Sample -> Contract -> SampleResult
    , getAnswer : Sample -> Answer
    }


type alias Model =
    { tests : List Test
    , samples : List Sample
    , contracts : List Contract
    , answers : List Answer
    , setup : SetupInterrogator
    , sampleColoring : Dict Sample Color
    , results : List SampleResult
    , answersRevealed : Bool
    , money : Int
    , selectedSample : Maybe Sample
    , resultsExpanded : Bool
    }


initialModel =
    { tests = []
    , samples = []
    , contracts = []
    , answers = []
    , sampleColoring = Dict.empty
    , results = []
    , setup = emptyInterrogator
    , answersRevealed = False
    , money = 10
    , selectedSample = Nothing
    , resultsExpanded = True
    }


type Msg
    = Noop
    | GotGameState (Result Http.Error GameSetup)
    | SelectNone
    | SampleClicked Sample
    | TestClicked Test
    | ContractClicked Contract
    | ShowAnswers
    | ToggleResults



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        SampleClicked sample ->
            let
                newSample =
                    pickOrToggle model.selectedSample sample
            in
            ( { model | selectedSample = newSample }, Cmd.none )

        SelectNone ->
            ( { model | selectedSample = Nothing }, Cmd.none )

        TestClicked test ->
            case model.selectedSample of
                Nothing ->
                    ( model, Cmd.none )

                Just sample ->
                    let
                        result =
                            model.setup.measure sample test

                        updatedMoney =
                            model.money - 1

                        newModel =
                            if List.length (List.filter (\x -> x == result) model.results) > 0 then
                                model

                            else
                                { model | results = model.results ++ [ result ], money = updatedMoney }
                    in
                    ( newModel, Cmd.none )

        ContractClicked contract ->
            case model.selectedSample of
                Nothing ->
                    ( model, Cmd.none )

                Just sample ->
                    let
                        result =
                            model.setup.tryContract sample contract

                        updatedMoney =
                            case result of
                                TestResult _ _ _ ->
                                    model.money

                                ContractResult _ _ success ->
                                    if success == True then
                                        model.money + 5

                                    else
                                        model.money - 5

                        newModel =
                            if List.length (List.filter (\x -> x == result) model.results) > 0 then
                                model

                            else
                                { model
                                    | results = model.results ++ [ result ]
                                    , money = updatedMoney
                                }
                    in
                    ( newModel, Cmd.none )

        GotGameState s ->
            case s of
                Ok setup ->
                    ( initializeModel model setup, Cmd.none )

                Err e ->
                    -- Do nothing
                    ( model, Cmd.none )

        ShowAnswers ->
            ( { model | answersRevealed = True }, Cmd.none )

        ToggleResults ->
            ( { model | resultsExpanded = not model.resultsExpanded }, Cmd.none )


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


initializeModel model setup =
    let
        { measures, contracts, answers } =
            setup

        samples : List Sample
        samples =
            Dict.keys measures

        tests : List Test
        tests =
            Dict.values measures
                |> List.head
                |> Maybe.withDefault Dict.empty
                |> Dict.keys

        foundAnswers : List Answer
        foundAnswers =
            List.map
                (\( sample, answer ) -> Answer sample answer)
                (Dict.toList answers)

        contractNames : List Contract
        contractNames =
            List.map Dict.keys (Dict.values contracts)
                |> List.head
                |> Maybe.withDefault []

        okColors =
            [ Color.red
            , Color.brown
            , Color.green
            , Color.blue
            , Color.purple
            , Color.orange
            ]

        sampleColoring : Dict Sample Color
        sampleColoring =
            Dict.fromList <|
                List.map2
                    Tuple.pair
                    samples
                    okColors

        interrogator : SetupInterrogator
        interrogator =
            { measure = measure setup
            , tryContract = tryContract setup
            , getAnswer = getAnswer setup
            }
    in
    { model
        | tests = tests
        , samples = samples
        , sampleColoring = sampleColoring
        , contracts = contractNames
        , answers = foundAnswers
        , setup = interrogator
    }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        possibleAnswers =
            if model.answersRevealed == True then
                [ viewAnswers model.answers ]

            else
                []
    in
    { title = "Strings"
    , body =
        [ div [] <|
            [ div [ Attrs.class "top-container" ]
                [ viewSamples model.samples model.selectedSample model.sampleColoring
                , viewSubmit model
                ]
            , viewTests model.tests
            , viewContracts model.contracts model.results model.sampleColoring
            , viewResults model.results model.resultsExpanded model.sampleColoring model.money
            ]
                ++ possibleAnswers
                ++ [ viewInstructions ]
        ]
    }


colorForSample sampleColoring sample =
    Dict.get sample sampleColoring |> Maybe.withDefault Color.black


bgColor : Color -> Attribute Msg
bgColor color =
    Color.toCssString color |> Attrs.style "background"


fgColor : Color -> Attribute Msg
fgColor color =
    Color.toCssString color |> Attrs.style "color"


viewSamples samples selectedSample sampleColoring =
    let
        getColor : Sample -> Color
        getColor =
            colorForSample sampleColoring
    in
    section []
        [ h1 [] [ text "Samples" ]
        , viewButtons
            { caption = \x -> x
            , signal = \x -> SampleClicked x
            , style =
                \x ->
                    case selectedSample of
                        Nothing ->
                            defaultButtonStyle ++ [ getColor x |> bgColor ]

                        Just selectedSample_ ->
                            if x == selectedSample_ then
                                selectedButtonStyle ++ [ getColor x |> bgColor ]

                            else
                                defaultButtonStyle ++ [ getColor x |> bgColor ]
            }
            samples
        ]


viewSubmit model =
    div [ Attrs.class "submit-area" ]
        [ viewButton "Reveal Answers" [ Attrs.class "answer-button" ] ShowAnswers
        ]


viewTests tests =
    section []
        [ h1 [] [ text "Tests" ]
        , viewButtons
            { caption = \x -> x
            , signal = \x -> TestClicked x
            , style = \_ -> defaultButtonStyle
            }
            tests
        ]


viewContracts contracts results sampleColoring =
    let
        getColor : Sample -> Color
        getColor =
            colorForSample sampleColoring

        yesses contract =
            List.filterMap
                (\res ->
                    case res of
                        ContractResult sample contract_ val ->
                            if val == True && contract == contract_ then
                                Just sample

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                results

        noes contract =
            List.filterMap
                (\res ->
                    case res of
                        ContractResult sample contract_ val ->
                            if val == False && contract == contract_ then
                                Just sample

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                results

        viewContract contract =
            let
                yesOutput =
                    List.map (\sample -> icon "fas fa-check" (getColor sample)) (yesses contract)

                noOutput =
                    List.map (\sample -> icon "fas fa-times" (getColor sample)) (noes contract)
            in
            button
                [ Attrs.class "contract", onClick (ContractClicked contract) ]
                [ text contract
                , span [] (yesOutput ++ noOutput)
                ]
    in
    section []
        [ h1 [] [ text "Contracts" ]
        , div [] (List.map viewContract contracts)
        ]


viewResults results expanded sampleColoring money =
    let
        getColor =
            colorForSample sampleColoring

        resultStyle sample =
            [ Attrs.class "outer", getColor sample |> fgColor ]

        viewResult : SampleResult -> Html Msg
        viewResult res =
            case res of
                TestResult sample test num ->
                    li
                        (resultStyle sample)
                        [ text (sample ++ " " ++ test ++ " = " ++ String.fromInt num) ]

                ContractResult sample contract p ->
                    let
                        truth =
                            if p == True then
                                "YES"

                            else
                                "NO"
                    in
                    li
                        (resultStyle sample ++ [ Attrs.style "font-weight" "bold" ])
                        [ text
                            (String.join " " [ sample, contract, "=", truth ])
                        ]

        exStyle =
            if expanded == True then
                { arrowClass = "fas fa-angle-down"
                , disp = Attrs.style "display" "block"
                }

            else
                { arrowClass = "fas fa-angle-up"
                , disp = Attrs.style "display" "none"
                }
    in
    div [ Attrs.class "footer" ]
        [ h1
            [ onClick ToggleResults ]
            [ icon exStyle.arrowClass Color.black
            , span [] [ text ("Results" ++ " (points: " ++ String.fromInt money ++ ")") ]
            ]
        , ol [ exStyle.disp ]
            (List.map viewResult results)
        ]


viewAnswers answers =
    section [ Attrs.class "answer-overlay" ]
        [ h1 [] [ text "Answers" ]
        , table [ Attrs.class "answer-table" ]
            (List.map
                (\(Answer sample answer) ->
                    tr [] [ td [] [ span [] [ text sample ], span [] [ text answer ] ] ]
                )
                answers
            )
        ]


viewInstructions =
    section []
        [ hr [] []
        , h1 [] [ text "Instructions" ]
        , div []
            [ h2 [] [ text "Goal" ]
            , p [] [ text "Get the most points you can by guessing the composition of the 'samples' and completing contracts." ]
            ]
        , div []
            [ h2 [] [ text "Samples" ]
            , p [] [ text "The samples are each secret 5-letter strings made of A, B, C, and D." ]
            ]
        , div []
            [ h2 [] [ text "Tests" ]
            , p [] [ text "Tests tell you information about samples.  Generally they tell you the number of times a certain substring appears." ]
            , p [] [ text "Each test costs one point to run, so try to use as few tests as possible!" ]
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
            [ h2 [] [ text "Contracts" ]
            , p [] [ text "Contracts are like tests, but instead of telling you information about a sample, you put a sample on a contract if you know it satisfies the conditions of the contract." ]
            , p [] [ text "Contracts look like tests, but they have comparisons in them.  " ]
            , p [] [ text "For example, ", b [] [ text "AAA..=3" ], text " is a contract that requires a sample where there are exactly 3 As at the beginning of the string" ]
            , p [] [ text "Contracts ", em [] [ text "make" ], text " you 5 points if you satisfy the contract, but if you're incorrect they ", em [] [ text "cost" ], text " you 5 points!  So try to avoid submitting samples for contracts unless you are pretty sure!" ]
            ]
        , div []
            [ h2 [] [ text "How to play" ]
            , p [] [ text "Click the sample you want to test, then the test you want to perform on it.  The answer will appear in the 'Results' list." ]
            , p [] [ text "Your points are listed at the top of the Results bar.  Right now the game allows negative points, but the goal is to get the ", em [] [ text "most" ], text " points you can!" ]
            , p [] [ text "When you're ready to check your work, click the 'Reveal Answers' button." ]
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
    button (buttonStyle ++ [ onClick signal ]) [ text txt ]


icon class color =
    i [ Attrs.class class, fgColor color, Attrs.alt "" ] []



-- HTTP


fetchConstantGame : Cmd Msg
fetchConstantGame =
    Http.get
        { url = "game.json"
        , expect = Http.expectJson GotGameState constantGameDecoder
        }


constantGameDecoder : D.Decoder GameSetup
constantGameDecoder =
    D.map3
        (\x y z -> { measures = x, answers = y, contracts = z })
        (D.at [ "measures" ] (D.dict (D.dict D.int)))
        (D.at [ "answers" ] (D.dict D.string))
        (D.at [ "contracts" ] (D.dict (D.dict D.bool)))



-- HIDING API DICTS


measure : GameSetup -> Sample -> Test -> SampleResult
measure setup sample test =
    TestResult
        sample
        test
        (Dict.get sample setup.measures
            |> Maybe.andThen (Dict.get test)
            |> Maybe.withDefault 0
        )


tryContract : GameSetup -> Sample -> Contract -> SampleResult
tryContract setup sample contract =
    ContractResult
        sample
        contract
        (Dict.get sample setup.contracts
            |> Maybe.andThen (Dict.get contract)
            |> Maybe.withDefault False
        )


getAnswer : GameSetup -> Sample -> Answer
getAnswer setup sample =
    Answer
        sample
        (Dict.get sample setup.answers
            |> Maybe.withDefault "(unknown)"
        )


emptyGameSetup : GameSetup
emptyGameSetup =
    { measures = Dict.empty
    , contracts = Dict.empty
    , answers = Dict.empty
    }


emptyInterrogator : SetupInterrogator
emptyInterrogator =
    { measure = measure emptyGameSetup
    , tryContract = tryContract emptyGameSetup
    , getAnswer = getAnswer emptyGameSetup
    }
