module SelectedNumberTests exposing (..)

import TestUtils exposing (..)
import Test exposing (..)
import CalculatedModel exposing (..)
import TimeTypes exposing (..)
import Expect


testsSelectedNumber : Test
testsSelectedNumber =
    describe "Selected number"
        [ test "Should be 00 for minute view when 0" <|
            \() ->
                let
                    model =
                        initialModel False 9 0

                    calculatedModel =
                        calculateModel <| { model | mode = Minutes }
                in
                    Expect.equal calculatedModel.selectedNumber "00"
        , test "Should not have zero in front for minute view below 10" <|
            \() ->
                let
                    model =
                        initialModel False 13 7

                    calculatedModel =
                        calculateModel <| { model | mode = Minutes }
                in
                    Expect.equal calculatedModel.selectedNumber "7"
        , test "Should not have zero for hour view below 10 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 3 6
                in
                    Expect.equal calculatedModel.selectedNumber "3"
        , test "Should be 12 for hour view at 12 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 12 25
                in
                    Expect.equal calculatedModel.selectedNumber "12"
        , test "Should be 12 for hour view at 0 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 0 25
                in
                    Expect.equal calculatedModel.selectedNumber "12"
        , test "Should be less than 12 for hour view above 12 on AM/PM view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel False 15 25
                in
                    Expect.equal calculatedModel.selectedNumber "3"
        , test "Should not contain zero for hour view below 10 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 3 25
                in
                    Expect.equal calculatedModel.selectedNumber "3"
        , test "Should be 12 for hour view at 12 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 12 25
                in
                    Expect.equal calculatedModel.selectedNumber "12"
        , test "Should be 00 for 0 hour on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 0 25
                in
                    Expect.equal calculatedModel.selectedNumber "00"
        , test "Should above 12 when hour is above 12 on 24h view" <|
            \() ->
                let
                    calculatedModel =
                        calculateModel <| initialModel True 22 25
                in
                    Expect.equal calculatedModel.selectedNumber "22"
        ]
