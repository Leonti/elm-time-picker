module Tests exposing (..)

import Test exposing (..)
import Expect
import TimePicker


all : Test
all =
    describe "CalculatedModel"
        [ tests24Hours
        , testsDigitalTimeHours24h
        , testsDigitalTimeHours12h
        ]


tests24Hours : Test
tests24Hours =
    let
        calculatedModel =
            TimePicker.calculateModel <| initialModel True 13 25
    in
        describe "24 hours initialModel"
            [ test "Time perios should be hidden" <|
                \() ->
                    Expect.equal calculatedModel.timePeriodSelected TimePicker.AM
            , test "Outer numbers should be correct" <|
                \() ->
                    Expect.equal calculatedModel.outerNumbers [ "12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11" ]
            , test "Time periods should not be shown" <|
                \() ->
                    Expect.equal calculatedModel.timePeriodShown False
            ]


testsDigitalTimeHours24h : Test
testsDigitalTimeHours24h =
    describe "Digital time hours display - 24 hours model"
        [ test "0 hours should be shown as \"00\"" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 0 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "00"
        , test "12 hours should be shown as \"12\"" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 12 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "12"
        , test "am hours hould be shown as-is" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 5 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "05"
        , test "pm hours hould be shown as-is" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 13 25
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
                        TimePicker.calculateModel <| initialModel False 0 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "12"
        , test "12 hours should be shown as \"12\"" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel False 12 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "12"
        , test "am hours hould be shown as-is" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel False 5 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "05"
        , test "pm hours hould be shown in 12h mode" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel False 13 25
                in
                    Expect.equal calculatedModel.digitalTimeHours "01"
        ]


initialModel : Bool -> Int -> Int -> TimePicker.Model
initialModel is24Hours hours minutes =
    let
        ( timePickerModel, timePickerCmd ) =
            TimePicker.init
                { is24Hours = is24Hours
                , hours = hours
                , minutes = minutes
                }
    in
        timePickerModel
