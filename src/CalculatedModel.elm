module CalculatedModel exposing (InputModel, CalculatedModel, SelectedTime, toSelectedTime, calculateModel, toAmHours, toPmHours)

import TimeTypes exposing (..)


type alias InputModel =
    { is24Hours : Bool
    , mode : Mode
    , hoursSelected : Int
    , minutesSelected : Int
    }


type alias SelectedTime =
    { hoursSelected : Int
    , minutesSelected : Int
    }


type alias CalculatedModel =
    { outerNumbers : List String
    , innerNumbers : Maybe (List String)
    , pointerAngle : Int
    , isShortPointer : Bool
    , selectedNumber : String
    , digitalTimeHours : String
    , digitalTimeMinutes : String
    , mode : Mode
    , timePeriodSelected : TimePeriod
    , timePeriodShown : Bool
    }


calculateModel : InputModel -> CalculatedModel
calculateModel model =
    { outerNumbers = outerNumbersToDisplay model.mode model.is24Hours
    , innerNumbers = innerNumbersToDisplay model.mode model.is24Hours
    , pointerAngle = toPointerAngle model.mode model.is24Hours model.hoursSelected model.minutesSelected
    , isShortPointer = (model.mode == Hours && model.is24Hours) && isInnerSelected model.hoursSelected
    , selectedNumber = toSelectedNumber model.mode model.is24Hours model.hoursSelected model.minutesSelected
    , digitalTimeHours = digitalTimeHoursToDisplay model.is24Hours model.hoursSelected
    , digitalTimeMinutes = doubleDigitFormat model.minutesSelected
    , mode = model.mode
    , timePeriodSelected = hoursToTimePeriod model.hoursSelected
    , timePeriodShown = not model.is24Hours
    }


outerNumbersToDisplay : Mode -> Bool -> List String
outerNumbersToDisplay mode is24Hours =
    case mode of
        Hours ->
            List.map toDoubleZeroString <| 12 :: (List.range 1 11)

        Minutes ->
            List.map doubleDigitFormat <| List.map (\x -> x * 5) <| List.range 0 11


innerNumbersToDisplay : Mode -> Bool -> Maybe (List String)
innerNumbersToDisplay mode is24Hours =
    case mode of
        Hours ->
            case is24Hours of
                True ->
                    Just <| List.map toDoubleZeroString <| 0 :: (List.range 13 23)

                False ->
                    Nothing

        Minutes ->
            Nothing


toDoubleZeroString : Int -> String
toDoubleZeroString number =
    if number == 0 then
        "00"
    else
        toString number


doubleDigitFormat : Int -> String
doubleDigitFormat number =
    if number < 10 then
        "0" ++ toString number
    else
        toString number


toPointerAngle : Mode -> Bool -> Int -> Int -> Int
toPointerAngle mode is24Hours hours minutes =
    case mode of
        Hours ->
            pointerAngle12 <| toAmHours hours

        Minutes ->
            pointerAngle60 minutes


isAm : Int -> Bool
isAm hours =
    if hours >= 0 && hours < 12 then
        True
    else
        False


toAmHours : Int -> Int
toAmHours hours =
    if hours == 0 || hours == 12 then
        12
    else if isAm hours then
        hours
    else
        hours - 12


toPmHours : Int -> Int
toPmHours hours =
    if isAm hours then
        hours + 12
    else
        hours


pointerAngle : Int -> Int -> Int
pointerAngle base value =
    let
        angle =
            round <| (360 / (toFloat base)) * (toFloat value)
    in
        if angle == 360 then
            0
        else
            angle


pointerAngle12 : Int -> Int
pointerAngle12 =
    pointerAngle 12


pointerAngle60 : Int -> Int
pointerAngle60 =
    pointerAngle 60


isInnerSelected : Int -> Bool
isInnerSelected selected =
    List.member selected (0 :: (List.range 13 23))


toSelectedNumber : Mode -> Bool -> Int -> Int -> String
toSelectedNumber mode is24Hours hours minutes =
    case mode of
        Hours ->
            if is24Hours then
                toDoubleZeroString hours
            else
                toDoubleZeroString <| toAmHours hours

        Minutes ->
            doubleDigitFormat minutes


digitalTimeHoursToDisplay : Bool -> Int -> String
digitalTimeHoursToDisplay is24Hours hours =
    if is24Hours then
        doubleDigitFormat hours
    else
        doubleDigitFormat <| toAmHours hours


hoursToTimePeriod : Int -> TimePeriod
hoursToTimePeriod hours =
    if hours >= 0 && hours < 12 then
        AM
    else
        PM


toSelectedTime : InputModel -> Selection -> SelectedTime
toSelectedTime model selection =
    case model.mode of
        Minutes ->
            let
                minutesRange =
                    List.range 0 59

                maybeMinute =
                    List.head (List.drop selection.angle minutesRange)
            in
                case maybeMinute of
                    Just selectedMinute ->
                        { hoursSelected = model.hoursSelected
                        , minutesSelected = selectedMinute
                        }

                    Nothing ->
                        { hoursSelected = model.hoursSelected
                        , minutesSelected = model.minutesSelected
                        }

        Hours ->
            case model.is24Hours of
                True ->
                    applySelection24h model selection

                False ->
                    applySelection12h model selection


applySelection24h : InputModel -> Selection -> SelectedTime
applySelection24h model selection =
    case selection.isInner of
        True ->
            let
                maybeHour =
                    List.head (List.drop selection.angle hoursInner)
            in
                case maybeHour of
                    Just selectedHour ->
                        { hoursSelected = selectedHour
                        , minutesSelected = model.minutesSelected
                        }

                    Nothing ->
                        { hoursSelected = model.hoursSelected
                        , minutesSelected = model.minutesSelected
                        }

        False ->
            let
                maybeHour =
                    List.head (List.drop selection.angle hoursOuter)
            in
                case maybeHour of
                    Just selectedHour ->
                        { hoursSelected = selectedHour
                        , minutesSelected = model.minutesSelected
                        }

                    Nothing ->
                        { hoursSelected = model.hoursSelected
                        , minutesSelected = model.minutesSelected
                        }


applySelection12h : InputModel -> Selection -> SelectedTime
applySelection12h model selection =
    let
        maybeHour =
            List.head (List.drop selection.angle hoursOuter)
    in
        case maybeHour of
            Just selectedHour ->
                if isAm model.hoursSelected then
                    let
                        adjustedHour =
                            if selectedHour == 12 then
                                0
                            else
                                selectedHour
                    in
                        { hoursSelected = adjustedHour
                        , minutesSelected = model.minutesSelected
                        }
                else
                    let
                        pmHour =
                            selectedHour + 12

                        adjustedPmHour =
                            if pmHour == 24 then
                                12
                            else
                                pmHour
                    in
                        { hoursSelected = adjustedPmHour
                        , minutesSelected = model.minutesSelected
                        }

            Nothing ->
                { hoursSelected = model.hoursSelected
                , minutesSelected = model.minutesSelected
                }


hoursOuter : List Int
hoursOuter =
    12 :: (List.range 1 11)


hoursInner : List Int
hoursInner =
    0 :: (List.range 13 23)
