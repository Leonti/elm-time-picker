module TestUtils exposing (initialModel)

import TimePicker


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
