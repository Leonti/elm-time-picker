module TestUtils exposing (initialModel)

import TimePicker
import CalculatedModel exposing (..)


initialModel : Bool -> Int -> Int -> InputModel
initialModel is24Hours hours minutes =
    let
        model =
            TimePicker.init
                { is24Hours = is24Hours
                , hours = hours
                , minutes = minutes
                , mainColor = ""
                }
    in
        { is24Hours = model.settings.is24Hours
        , mode = model.mode
        , hoursSelected = model.hoursSelected
        , minutesSelected = model.minutesSelected
        }
