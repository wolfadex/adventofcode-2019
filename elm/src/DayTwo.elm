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


type Model
    = Running RunningModel
    | Complete (Array Int)
    | Error String


type alias RunningModel =
    { input : Array Int
    , index : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Running { input = puzzleInput, index = 0 }, process )


process : Cmd Msg
process =
    Task.perform (\_ -> Process) (Task.succeed ())


type Msg
    = Process


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Running { input, index } ->
            case msg of
                Process ->
                    case Array.get index input of
                        Nothing ->
                            ( Error "Missing index", Cmd.none )

                        Just 99 ->
                            ( Complete input, Cmd.none )

                        Just 1 ->
                            case getCalcParts index input of
                                Nothing ->
                                    ( Error "Missing calc parts", Cmd.none )

                                Just ( aIdx, bIdx, resultIdx ) ->
                                    nextInput (mathOnIndicies aIdx bIdx resultIdx (+) input) index

                        Just 2 ->
                            case getCalcParts index input of
                                Nothing ->
                                    ( Error "Missing calc parts", Cmd.none )

                                Just ( aIdx, bIdx, resultIdx ) ->
                                    nextInput (mathOnIndicies aIdx bIdx resultIdx (*) input) index

                        Just a ->
                            ( Error ("Unknown opcode: " ++ String.fromInt a), Cmd.none )

        _ ->
            ( model, Cmd.none )


mathOnIndicies : Int -> Int -> Int -> (Int -> Int -> Int) -> Array Int -> Array Int
mathOnIndicies aIdx bIdx resultIdx math array =
    case ( Array.get aIdx array, Array.get bIdx array ) of
        ( Just a, Just b ) ->
            Array.set resultIdx (math a b) array

        _ ->
            array


nextInput : Array Int -> Int -> ( Model, Cmd Msg )
nextInput next index =
    ( Running { index = index + 4, input = next }, process )


getCalcParts : Int -> Array a -> Maybe ( a, a, a )
getCalcParts index array =
    case ( Array.get (index + 1) array, Array.get (index + 2) array, Array.get (index + 3) array ) of
        ( Just a, Just b, Just c ) ->
            Just ( a, b, c )

        _ ->
            Nothing


view : Model -> Html Msg
view model =
    Element.layout
        []
        (Element.column
            []
            (case model of
                Error err ->
                    [ Element.text err ]

                Running _ ->
                    [ Element.text "Computing" ]

                Complete answer ->
                    case Array.get 0 answer of
                        Nothing ->
                            [ Element.text "ERROR" ]

                        Just a ->
                            [ Element.text ("Part 1: " ++ String.fromInt a) ]
            )
        )


puzzleInput : Array Int
puzzleInput =
    Array.fromList
        [ 1
        , 12 -- 0
        , 2 -- 0
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
