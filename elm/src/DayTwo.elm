module DayTwo exposing (main)

import Array exposing (Array)
import Browser
import Element exposing (Element)
import Html exposing (Html)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { part1 : State, part2 : State }


type State
    = Running RunningState
    | Complete RunningState
    | Error String


type alias RunningState =
    { input : Array Int
    , instructionPointer : Int
    , noun : Int
    , verb : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { part1 = Running { input = puzzleInput |> setNoun 12 |> setVerb 2, instructionPointer = 0, noun = 12, verb = 2 }
      , part2 = Running { input = puzzleInput, instructionPointer = 0, noun = 0, verb = 0 }
      }
    , process
    )


process : Cmd Msg
process =
    Task.perform (\_ -> Process) (Task.succeed ())


type Msg
    = Process


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Process ->
            let
                ( nextPart1, cmd1 ) =
                    case model.part1 of
                        Running runningState ->
                            computer runningState

                        _ ->
                            ( model.part1, Cmd.none )

                ( nextPart2, cmd2 ) =
                    case model.part2 of
                        Running runningState ->
                            computer runningState

                        _ ->
                            ( model.part2, Cmd.none )

                ( finalPart2, cmd2Final ) =
                    case nextPart2 of
                        Complete completeState ->
                            if checkGoal completeState.input then
                                ( nextPart2, cmd2 )

                            else
                                ( Running ({ completeState | input = puzzleInput, instructionPointer = 0 } |> incrementNounVerb), process )

                        _ ->
                            ( nextPart2, cmd2 )
            in
            ( { model | part1 = nextPart1, part2 = finalPart2 }
            , Cmd.batch [ cmd1, cmd2Final ]
            )


incrementNounVerb : RunningState -> RunningState
incrementNounVerb stateOrig =
    let
        stateNounInc =
            { stateOrig | noun = stateOrig.noun + 1 }

        stateIncremented =
            if stateNounInc.noun > 99 then
                { stateOrig | noun = 0, verb = stateOrig.verb + 1 }

            else
                stateNounInc
    in
    { stateIncremented
        | input =
            stateIncremented.input
                |> setNoun stateIncremented.noun
                |> setVerb stateIncremented.verb
    }


computer : RunningState -> ( State, Cmd Msg )
computer ({ instructionPointer, input } as runningState) =
    case Array.get instructionPointer input of
        Nothing ->
            ( Error "Missing instructionPointer", Cmd.none )

        Just 99 ->
            ( Complete runningState, Cmd.none )

        Just 1 ->
            case getCalcParts instructionPointer input of
                Nothing ->
                    ( Error "Missing calc parts", Cmd.none )

                Just ( aIdx, bIdx, resultIdx ) ->
                    ( nextInput (mathOnIndicies aIdx bIdx resultIdx (+) input) runningState, process )

        Just 2 ->
            case getCalcParts instructionPointer input of
                Nothing ->
                    ( Error "Missing calc parts", Cmd.none )

                Just ( aIdx, bIdx, resultIdx ) ->
                    ( nextInput (mathOnIndicies aIdx bIdx resultIdx (*) input) runningState, process )

        Just a ->
            ( Error ("Unknown opco\n            : " ++ String.fromInt a), Cmd.none )


mathOnIndicies : Int -> Int -> Int -> (Int -> Int -> Int) -> Array Int -> Array Int
mathOnIndicies aIdx bIdx resultIdx math array =
    case ( Array.get aIdx array, Array.get bIdx array ) of
        ( Just a, Just b ) ->
            Array.set resultIdx (math a b) array

        _ ->
            array


nextInput : Array Int -> RunningState -> State
nextInput next state =
    Running { state | instructionPointer = state.instructionPointer + 4, input = next }


getCalcParts : Int -> Array a -> Maybe ( a, a, a )
getCalcParts index array =
    case ( Array.get (index + 1) array, Array.get (index + 2) array, Array.get (index + 3) array ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


view : Model -> Html Msg
view { part1, part2 } =
    Element.layout
        []
        (Element.column
            []
            [ case part1 of
                Error err ->
                    Element.text err

                Running _ ->
                    Element.text "Computing"

                Complete { input } ->
                    case Array.get 0 input of
                        Nothing ->
                            Element.text "ERROR"

                        Just a ->
                            Element.text ("Part 1: " ++ String.fromInt a)
            , case part2 of
                Error err ->
                    Element.text err

                Running _ ->
                    Element.text "Computing"

                Complete { noun, verb } ->
                    Element.text
                        ("Part 2: "
                            ++ String.fromInt (100 * noun + verb)
                            ++ ", noun = "
                            ++ String.fromInt noun
                            ++ ", verb = "
                            ++ String.fromInt verb
                        )
            ]
        )


setNoun : Int -> Array Int -> Array Int
setNoun =
    Array.set 1


setVerb : Int -> Array Int -> Array Int
setVerb =
    Array.set 2


goalValue : Int
goalValue =
    19690720


checkGoal : Array Int -> Bool
checkGoal array =
    array
        |> Array.get 0
        |> Maybe.withDefault -1
        |> (==) goalValue


puzzleInput : Array Int
puzzleInput =
    Array.fromList
        [ 1
        , 0
        , 0
        , 3
        , 1
        , 1
        , 2
        , 3
        , 1
        , 3
        , 4
        , 3
        , 1
        , 5
        , 0
        , 3
        , 2
        , 13
        , 1
        , 19
        , 1
        , 19
        , 10
        , 23
        , 1
        , 23
        , 13
        , 27
        , 1
        , 6
        , 27
        , 31
        , 1
        , 9
        , 31
        , 35
        , 2
        , 10
        , 35
        , 39
        , 1
        , 39
        , 6
        , 43
        , 1
        , 6
        , 43
        , 47
        , 2
        , 13
        , 47
        , 51
        , 1
        , 51
        , 6
        , 55
        , 2
        , 6
        , 55
        , 59
        , 2
        , 59
        , 6
        , 63
        , 2
        , 63
        , 13
        , 67
        , 1
        , 5
        , 67
        , 71
        , 2
        , 9
        , 71
        , 75
        , 1
        , 5
        , 75
        , 79
        , 1
        , 5
        , 79
        , 83
        , 1
        , 83
        , 6
        , 87
        , 1
        , 87
        , 6
        , 91
        , 1
        , 91
        , 5
        , 95
        , 2
        , 10
        , 95
        , 99
        , 1
        , 5
        , 99
        , 103
        , 1
        , 10
        , 103
        , 107
        , 1
        , 107
        , 9
        , 111
        , 2
        , 111
        , 10
        , 115
        , 1
        , 115
        , 9
        , 119
        , 1
        , 13
        , 119
        , 123
        , 1
        , 123
        , 9
        , 127
        , 1
        , 5
        , 127
        , 131
        , 2
        , 13
        , 131
        , 135
        , 1
        , 9
        , 135
        , 139
        , 1
        , 2
        , 139
        , 143
        , 1
        , 13
        , 143
        , 0
        , 99
        , 2
        , 0
        , 14
        , 0
        ]
