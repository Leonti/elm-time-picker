module Tests exposing (..)

import Test exposing (..)
import Expect
import CalculatedModel exposing (..)
import TimeTypes exposing (..)
import TestUtils exposing (..)
import PointerTests
import SelectedNumberTests
import PointerAngleTests
import SelectedTimeTests


all : Test
all =
    describe "CalculatedModel"
        [ tests24Hours
        , tests12Hours
        , testsMinutes
        , testsDigitalTimeHours24h
        , testsDigitalTimeHours12h
        , testsDigitalTimeMinutes
        , testsSelectedTimePeriod
        , PointerTests.testsPointerLength
        , SelectedNumberTests.testsSelectedNumber
        , PointerAngleTests.testsPointerAngle
        , SelectedTimeTests.testsSelectedTime
        ]


tests24Hours : Test
tests24Hours =
    let
        calculatedModel =
            calculateModel <| initialModel True 13 25
    in
        describe "24 hours initialModel"
            [ test "Outer numbers should be correct" <|
                \() ->
                    Expect.equal calculatedModel.outerNumbers
                        [ "12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11" ]
            , test "Inner numbers should be correct" <|
                \() ->
                    Expect.equal calculatedModel.innerNumbers <|
                        Just [ "00", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23" ]
            , test "Time periods should not be shown" <|
                \() ->
                    Expect.equal calculatedModel.timePeriodShown False
            , test "Mode should be Hours" <|
                \() ->
                    Expect.equal calculatedModel.mode Hours
            ]


tests12Hours : Test
tests12Hours =
    let
        calculatedModel =
            calculateModel <| initialModel False 13 25
    in
        describe "12 hours initialModel"
            [ test "Outer numbers should be correct" <|
                \() ->
                    Expect.equal calculatedModel.outerNumbers
                        [ "12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11" ]
            , test "Inner numbers should not be shown" <|
                \() ->
                    Expect.equal calculatedModel.innerNumbers Nothing
            , test "Time periods should be shown" <|
                \() ->
                    Expect.equal calculatedModel.timePeriodShown True
            , test "Mode should be Hours" <|
                \() ->
                    Expect.equal calculatedModel.mode Hours
            ]


testsMinutes : Test
testsMinutes =
    let
        model =
            initialModel False 13 25

        calculatedModel =
            calculateModel <| { model | mode = Minutes }
    in
        describe "minutes view"
            [ test "Outer numbers should be correct" <|
                \() ->
                    Expect.equal calculatedModel.outerNumbers
                        [ "00", "05", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55" ]
            , test "Inner numbers should not be shown" <|
                \() ->
                    Expect.equal calculatedModel.innerNumbers Nothing
            , test "Time periods should be shown" <|
                \() ->
                    Expect.equal calculatedModel.timePeriodShown True
            , test "Mode should be Hours" <|
                \() ->
                    Expect.equal calculatedModel.mode Minutes
            ]


testsDigitalTimeHours24h : Test
testsDigitalTimeHours24h =
    describe "Digital time hours display - 24 hours model"
        [ test "0 hours should be shown as \"00\"" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 0 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "00"
        , test "12 hours should be shown as \"12\"" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 12 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "12"
        , test "am hours hould be shown as-is" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 5 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "05"
        , test "pm hours hould be shown as-is" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 13 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "13"
        ]


testsDigitalTimeHours12h : Test
testsDigitalTimeHours12h =
    describe "Digital time hours display - 12 hours model"
        [ test "0 hours should be shown as \"12\"" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 0 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "12"
        , test "12 hours should be shown as \"12\"" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 12 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "12"
        , test "am hours hould be shown as-is" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 5 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "05"
        , test "pm hours hould be shown in 12h mode" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 13 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "01"
        ]


testsDigitalTimeMinutes : Test
testsDigitalTimeMinutes =
    describe "Digital time minutes display"
        [ test "0 minutes should be shown as \"00\"" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 0 0
                in
                    Expect.equal calculatedModel.digitalTimeMinutes "00"
        , test "minutes less than 10 should have 0 prepended" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 0 5
                in
                    Expect.equal calculatedModel.digitalTimeMinutes "05"
        ]


testsSelectedTimePeriod : Test
testsSelectedTimePeriod =
    describe "Selected time period"
        [ test "0 hours should be AM" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 0 0
                in
                    Expect.equal calculatedModel.timePeriodSelected AM
        , test "12 hours should be PM" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 12 0
                in
                    Expect.equal calculatedModel.timePeriodSelected PM
        , test "7 hours should be AM" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 7 0
                in
                    Expect.equal calculatedModel.timePeriodSelected AM
        , test "13 hours should be PM" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 13 0
                in
                    Expect.equal calculatedModel.timePeriodSelected PM
        ]
