module DayOne exposing (main)

import Element exposing (Element)
import Html exposing (Html)


main : Html msg
main =
    Element.layout
        []
        (Element.column
            []
            [ viewSolution
            , viewSolution2
            ]
        )


viewSolution : Element msg
viewSolution =
    List.foldl
        (\mass total ->
            total + (mass // 3 - 2)
        )
        0
        input
        |> String.fromInt
        |> (++) "Part 1: "
        |> Element.text


viewSolution2 : Element msg
viewSolution2 =
    List.foldl
        (\mass total -> compute mass + total)
        0
        input
        |> String.fromInt
        |> (++) "Part 2: "
        |> Element.text


compute : Int -> Int
compute mass =
    let
        fuel =
            mass // 3 - 2
    in
    computeHelper fuel (fuel // 3 - 2)


computeHelper : Int -> Int -> Int
computeHelper total minimum =
    if minimum < 0 then
        total

    else
        computeHelper (total + minimum) (minimum // 3 - 2)


input : List Int
input =
    [ 102562
    , 138390
    , 145043
    , 86679
    , 120601
    , 58443
    , 54761
    , 81175
    , 127897
    , 69559
    , 56776
    , 145671
    , 69003
    , 119334
    , 130205
    , 77249
    , 74637
    , 92068
    , 66594
    , 90485
    , 140465
    , 73444
    , 107772
    , 107639
    , 144420
    , 58764
    , 56299
    , 66010
    , 84841
    , 83686
    , 139830
    , 136298
    , 135009
    , 136506
    , 61547
    , 73653
    , 136219
    , 138875
    , 95483
    , 91695
    , 146597
    , 121813
    , 131555
    , 145848
    , 139396
    , 141520
    , 54207
    , 86748
    , 98355
    , 67179
    , 59820
    , 137299
    , 92371
    , 74512
    , 110854
    , 111960
    , 63787
    , 114701
    , 63773
    , 127377
    , 128159
    , 120370
    , 138193
    , 106409
    , 135550
    , 107235
    , 56662
    , 99314
    , 69052
    , 131816
    , 138788
    , 96494
    , 73025
    , 148907
    , 85883
    , 86138
    , 86965
    , 55645
    , 119284
    , 80690
    , 69276
    , 116640
    , 108595
    , 50721
    , 94623
    , 93224
    , 137069
    , 130118
    , 97916
    , 82232
    , 137621
    , 97909
    , 74061
    , 140419
    , 101795
    , 69316
    , 64973
    , 90578
    , 118503
    , 100369
    ]
