module SelectedTimeTests exposing (..)

import TestUtils exposing (..)
import Test exposing (..)
import CalculatedModel exposing (..)
import TimeTypes exposing (..)
import Expect


testsSelectedTime : Test
testsSelectedTime =
    describe "Selected time"
        [ testsMinutes
        , tests12Hours
        , tests24Hours
        ]


calculateTestSelectedTime : Mode -> Bool -> Int -> Selection -> SelectedTime
calculateTestSelectedTime mode is24Hours hours selection =
    let
        model =
            initialModel is24Hours hours 0

        modelWithMode =
            { model | mode = mode }
    in
        toSelectedTime modelWithMode selection


testsMinutes : Test
testsMinutes =
    describe "Minutes"
        [ test "Should be 0 when angle is 0" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Minutes False 0 { angle = 0, isInner = False }
                in
                    Expect.equal selectedTime.minutesSelected 0
        , test "Should be 15 when angle is 15" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Minutes False 0 { angle = 15, isInner = False }
                in
                    Expect.equal selectedTime.minutesSelected 15
        ]


tests12Hours : Test
tests12Hours =
    describe "12h hours"
        [ test "Should be 12 when angle is 0 for PM" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours False 13 { angle = 0, isInner = False }
                in
                    Expect.equal selectedTime.hoursSelected 12
        , test "Should be 15 when angle is 3 for PM" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours False 13 { angle = 3, isInner = False }
                in
                    Expect.equal selectedTime.hoursSelected 15
        , test "Should be 0 when angle is 0 for AM" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours False 1 { angle = 0, isInner = False }
                in
                    Expect.equal selectedTime.hoursSelected 0
        , test "Should be 3 when angle is 3 for AM" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours False 1 { angle = 3, isInner = False }
                in
                    Expect.equal selectedTime.hoursSelected 3
        ]


tests24Hours : Test
tests24Hours =
    describe "24h hours"
        [ test "Should be 12 when angle is 0 for outer" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours True 13 { angle = 0, isInner = False }
                in
                    Expect.equal selectedTime.hoursSelected 12
        , test "Should be 3 when angle is 3 for outer" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours True 13 { angle = 3, isInner = False }
                in
                    Expect.equal selectedTime.hoursSelected 3
        , test "Should be 0 when angle is 0 for inner" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours True 1 { angle = 0, isInner = True }
                in
                    Expect.equal selectedTime.hoursSelected 0
        , test "Should be 15 when angle is 3 for inner" <|
            \() ->
                let
                    selectedTime =
                        calculateTestSelectedTime Hours True 1 { angle = 3, isInner = True }
                in
                    Expect.equal selectedTime.hoursSelected 15
        ]
