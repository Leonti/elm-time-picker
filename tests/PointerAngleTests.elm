module PointerAngleTests exposing (..)

import TestUtils exposing (..)
import Test exposing (..)
import CalculatedModel exposing (..)
import TimeTypes exposing (..)
import Expect


testsPointerAngle : Test
testsPointerAngle =
    describe "Selected number"
        [ testsMinutes
        , tests12Hours
        , tests24Hours
        ]


testsMinutes : Test
testsMinutes =
    describe "Minutes"
        [ test "Should 0 when 0" <|
            \() ->
                let
                    model =
                        initialModel False 9 0

                    calculatedModel =
                        calculateModel <| { model | mode = Minutes }
                in
                    Expect.equal calculatedModel.pointerAngle 0
        , test "Should be multiplied by 6" <|
            \() ->
                let
                    model =
                        initialModel False 13 7

                    calculatedModel =
                        calculateModel <| { model | mode = Minutes }
                in
                    Expect.equal calculatedModel.pointerAngle 42
        ]


tests12Hours : Test
tests12Hours =
    describe "12h hours"
        [ test "Should be multiplied by 30" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 3 6
                in
                    Expect.equal calculatedModel.pointerAngle 90
        , test "Should be 0 for hour view at 12" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 12 25
                in
                    Expect.equal calculatedModel.pointerAngle 0
        , test "Should be 0 for hour view at 0" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 0 25
                in
                    Expect.equal calculatedModel.pointerAngle 0
        , test "Should rotate angle for hour view above 12" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 15 25
                in
                    Expect.equal calculatedModel.pointerAngle 90
        ]


tests24Hours : Test
tests24Hours =
    describe "24h hours"
        [ test "Should be multiplied by 30" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 3 6
                in
                    Expect.equal calculatedModel.pointerAngle 90
        , test "Should be 0 for hour view at 12" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 12 25
                in
                    Expect.equal calculatedModel.pointerAngle 0
        , test "Should be 0 for hour view at 0" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 0 25
                in
                    Expect.equal calculatedModel.pointerAngle 0
        , test "Should rotate angle for hour view above 12" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 15 25
                in
                    Expect.equal calculatedModel.pointerAngle 90
        ]
