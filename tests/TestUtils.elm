module TestUtils exposing (initialModel)

import TimePicker
import CalculatedModel exposing (..)
import Date.Extra.Core exposing (intToMonth)
import Date.Extra.Create exposing (dateFromFields)
import Date exposing (Date, hour, minute)


initialModel : Bool -> Int -> Int -> InputModel
initialModel is24Hours hours minutes =
    let
        date =
            (dateFromFields 2017 (intToMonth 2) 8 hours minutes 0 0)

        model =
            TimePicker.init
                { is24Hours = is24Hours
                , mainColor = ""
                }
                date
    in
        { is24Hours = model.settings.is24Hours
        , mode = model.mode
        , hoursSelected = hour date
        , minutesSelected = minute date
        }
