module Main exposing (..)

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
import Json.Encode as E
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
init () =
    ( Loading, makeNewGame )


resetGame : GameId -> ( Model, Cmd Msg )
resetGame id =
    ( Loading, fetchGame id )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- TYPES


type alias GameId =
    String


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


type alias PlayingGameData =
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


type Model
    = PlayingGame PlayingGameData
    | Loading


type Msg
    = Noop
    | GotGameState (Result Http.Error GameSetup)
    | NewGameCreated (Result Http.Error GameId)
    | SelectNone
    | SampleClicked Sample
    | TestClicked Test
    | ContractClicked Contract
    | ShowAnswers
    | RequestNewGame
    | ToggleResults



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Noop, _ ) ->
            ( model, Cmd.none )

        ( SampleClicked sample, PlayingGame data ) ->
            let
                newSample =
                    pickOrToggle data.selectedSample sample
            in
            ( PlayingGame { data | selectedSample = newSample }, Cmd.none )

        ( SelectNone, PlayingGame data ) ->
            ( PlayingGame { data | selectedSample = Nothing }, Cmd.none )

        ( TestClicked test, PlayingGame data ) ->
            case data.selectedSample of
                Nothing ->
                    ( PlayingGame data, Cmd.none )

                Just sample ->
                    let
                        result =
                            data.setup.measure sample test

                        updatedMoney =
                            data.money - 1

                        newData =
                            if List.length (List.filter (\x -> x == result) data.results) > 0 then
                                data

                            else if data.answersRevealed == True then
                                data

                            else
                                { data | results = data.results ++ [ result ], money = updatedMoney }
                    in
                    ( PlayingGame newData, Cmd.none )

        ( ContractClicked contract, PlayingGame data ) ->
            case data.selectedSample of
                Nothing ->
                    ( PlayingGame data, Cmd.none )

                Just sample ->
                    let
                        result =
                            data.setup.tryContract sample contract

                        updatedMoney =
                            case result of
                                TestResult _ _ _ ->
                                    data.money

                                ContractResult _ _ success ->
                                    if success == True then
                                        data.money + 5

                                    else
                                        data.money - 5

                        newData =
                            if List.length (List.filter (\x -> x == result) data.results) > 0 then
                                data

                            else if data.answersRevealed == True then
                                data

                            else
                                { data
                                    | results = data.results ++ [ result ]
                                    , money = updatedMoney
                                }
                    in
                    ( PlayingGame newData, Cmd.none )

        ( GotGameState (Ok setup), _ ) ->
            ( initializeModel setup, Cmd.none )

        ( GotGameState (Err _), _ ) ->
            -- Do nothing
            ( model, Cmd.none )

        ( NewGameCreated (Ok id), _ ) ->
            resetGame id

        ( NewGameCreated (Err _), _ ) ->
            -- Do nothing
            ( model, Cmd.none )

        ( ShowAnswers, PlayingGame data ) ->
            ( PlayingGame { data | answersRevealed = True }, Cmd.none )

        ( ToggleResults, PlayingGame data ) ->
            ( PlayingGame { data | resultsExpanded = not data.resultsExpanded }, Cmd.none )

        ( RequestNewGame, _ ) ->
            ( Loading, makeNewGame )

        ( _, Loading ) ->
            -- Do nothing
            ( model, Cmd.none )


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


initializeModel setup =
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
    PlayingGame
        { tests = tests
        , samples = samples
        , sampleColoring = sampleColoring
        , contracts = contractNames
        , answers = foundAnswers
        , setup = interrogator
        , answersRevealed = False
        , money = 10
        , results = []
        , resultsExpanded = False
        , selectedSample = Nothing
        }



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        PlayingGame subModel ->
            viewGame subModel

        Loading ->
            div [] [ h1 [] [ text "Loading..." ] ]


viewGame : PlayingGameData -> Html Msg
viewGame model =
    let
        samplesOrAnswers =
            if model.answersRevealed == True then
                viewAnswers model.answers model.sampleColoring

            else
                viewSamples model.samples model.selectedSample model.sampleColoring

        submitOrNew =
            if model.answersRevealed == True then
                viewNewGame model

            else
                viewSubmit model
    in
    div [] <|
        [ div [ Attrs.class "top-container" ]
            [ samplesOrAnswers
            , submitOrNew
            ]
        , viewTests model.tests
        , viewContracts model.contracts model.results model.sampleColoring
        , viewResults model.results model.resultsExpanded model.sampleColoring model.money
        ]
            ++ [ viewInstructions ]


colorForSample sampleColoring sample =
    Dict.get sample sampleColoring |> Maybe.withDefault Color.black


bgColor : Color -> Attribute Msg
bgColor color =
    Attrs.style "background" (Color.toCssString color)


fgColor : Color -> Attribute Msg
fgColor color =
    Attrs.style "color" (Color.toCssString color)


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
                            [ Attrs.class "button", bgColor (getColor x) ]

                        Just selectedSample_ ->
                            if x == selectedSample_ then
                                [ Attrs.class "button", Attrs.class "bordered", bgColor (getColor x) ]

                            else
                                [ Attrs.class "button", bgColor (getColor x) ]
            }
            samples
        ]


viewSubmit model =
    div [ Attrs.class "submit-area" ]
        [ viewButton "Reveal Answers" [ Attrs.class "answer-button" ] ShowAnswers
        ]


viewNewGame model =
    div [ Attrs.class "submit-area" ]
        [ viewButton "New Game" [ Attrs.class "answer-button" ] RequestNewGame
        ]


viewTests tests =
    section []
        [ h1 [] [ text "Tests" ]
        , viewButtons
            { caption = \x -> x
            , signal = \x -> TestClicked x
            , style = \_ -> [ Attrs.class "button" ]
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
        , div [ Attrs.class "scroller-outer" ]
            [ ol [ exStyle.disp, Attrs.class "scroller-inner" ]
                (List.map viewResult results)
            ]
        ]


viewAnswers answers sampleColoring =
    let
        getColor : Sample -> Color
        getColor =
            colorForSample sampleColoring

        viewAnswer (Answer sample answer) =
            button
                [ bgColor (getColor sample)
                , Attrs.class "answer-sample"
                ]
                [ text sample, span [] [ text answer ] ]
    in
    section []
        [ h1 [] [ text "Answers" ]
        , div [] (List.map viewAnswer answers)
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


viewButtons : { caption : a -> String, signal : a -> Msg, style : a -> List (Attribute Msg) } -> List a -> Html Msg
viewButtons { caption, signal, style } buttons =
    div [ Attrs.class "section" ]
        (List.map
            (\x -> viewButton (caption x) (style x) (signal x))
            buttons
        )


viewButtonContainer : List (Html Msg) -> List (Attribute Msg) -> Msg -> Html Msg
viewButtonContainer content buttonStyle signal =
    button (buttonStyle ++ [ onClick signal ]) content


viewButton : String -> List (Attribute Msg) -> Msg -> Html Msg
viewButton txt buttonStyle signal =
    viewButtonContainer [ text txt ] buttonStyle signal


icon class color =
    i [ Attrs.class class, fgColor color, Attrs.alt "" ] []



-- HTTP


fetchConstantGame : Cmd Msg
fetchConstantGame =
    Http.get
        { url = "game.json"
        , expect = Http.expectJson GotGameState constantGameDecoder
        }


fetchGame : GameId -> Cmd Msg
fetchGame id =
    Http.get
        { url = "http://localhost:8008/games/" ++ id
        , expect = Http.expectJson GotGameState constantGameDecoder
        }


constantGameDecoder : D.Decoder GameSetup
constantGameDecoder =
    D.map3
        (\x y z -> { measures = x, answers = y, contracts = z })
        (D.at [ "measures" ] (D.dict (D.dict D.int)))
        (D.at [ "answers" ] (D.dict D.string))
        (D.at [ "contracts" ] (D.dict (D.dict D.bool)))


makeNewGame : Cmd Msg
makeNewGame =
    Http.post
        { url = "http://localhost:8008/games/"
        , expect = Http.expectJson NewGameCreated newGameCreationDecoder
        , body = Http.jsonBody newGameBody
        }


newGameBody : E.Value
newGameBody =
    E.object
        [ ( "alphabet", E.string "ABCD" )
        , ( "length", E.int 5 )
        , ( "samples", E.int 5 )
        , ( "contracts", E.int 10 )
        ]


newGameCreationDecoder : D.Decoder GameId
newGameCreationDecoder =
    D.at [ "data", "subject" ] D.string



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
