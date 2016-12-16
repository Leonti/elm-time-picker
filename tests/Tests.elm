module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import TimePicker


all : Test
all =
    describe "CalculatedModel"
        [ tests24Hours
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
