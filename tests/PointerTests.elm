module PointerTests exposing (..)

import TestUtils exposing (..)
import Test exposing (..)
import TimePicker
import Expect


testsPointerLength : Test
testsPointerLength =
    describe "Pointer length"
        [ test "Should be long for minute view less then 12" <|
            \() ->
                let
                    model =
                        initialModel False 9 2

                    calculatedModel =
                        TimePicker.calculateModel <| { model | mode = TimePicker.Minutes }
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be long for minute view more than 12" <|
            \() ->
                let
                    model =
                        initialModel False 13 25

                    calculatedModel =
                        TimePicker.calculateModel <| { model | mode = TimePicker.Minutes }
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be long for hour view below 12 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel False 3 25
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be long for hour view at 12 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel False 12 25
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be long for hour view above 12 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel False 15 25
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be long for hour view below 12 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 3 25
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be long for hour view at 12 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 12 25
                in
                    Expect.equal calculatedModel.isShortPointer False
        , test "Should be short for hour above 12 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 15 25
                in
                    Expect.equal calculatedModel.isShortPointer True
        , test "Should be short for 0 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        TimePicker.calculateModel <| initialModel True 0 25
                in
                    Expect.equal calculatedModel.isShortPointer True
        ]
