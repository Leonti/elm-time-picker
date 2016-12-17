module TimePicker exposing (Model, Msg, init, update, view, calculateModel, Mode(..), TimePeriod(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MousePosition exposing (..)


type alias Position =
    { x : Float
    , y : Float
    }


type alias Selection =
    { angle : Int
    , isInner : Bool
    }


type Mode
    = Hours
    | Minutes


type alias Settings =
    { is24Hours : Bool
    , hours : Int
    , minutes : Int
    }


type TimePeriod
    = AM
    | PM


outerPositions : List Position
outerPositions =
    [ Position 0 5
    , Position 54.5 16.6
    , Position 94.4 59.5
    , Position 109 114
    , Position 94.4 168.5
    , Position 54.5 208.4
    , Position 0 223
    , Position -54.5 208.4
    , Position -94.4 168.5
    , Position -109 114
    , Position -94.4 59.5
    , Position -54.5 19.6
    ]


innerPositions : List Position
innerPositions =
    [ Position 0 40
    , Position 36.9 49.9
    , Position 64 77
    , Position 74 114
    , Position 64 151
    , Position 37 178
    , Position 0 188
    , Position -37 178
    , Position -64 151
    , Position -74 114
    , Position -64 77
    , Position -37 50
    ]


minutes : List Int
minutes =
    List.map (\x -> x * 5) <| List.range 0 11


hoursOuter : List Int
hoursOuter =
    12 :: (List.range 1 11)


hoursInner : List Int
hoursInner =
    0 :: (List.range 13 23)


modeToStep : Mode -> Int
modeToStep mode =
    case mode of
        Hours ->
            30

        Minutes ->
            6


type alias Model =
    { settings : Settings
    , mode : Mode
    , hoursSelected : Int
    , minutesSelected : Int
    , isSelecting : Bool
    }


type alias CalculatedModel =
    { outerNumbers : List String
    , innerNumbers : Maybe (List String)
    , digitalTimeHours : String
    , digitalTimeMinutes : String
    , digitalTimeSelected : Mode
    , timePeriodSelected : TimePeriod
    , timePeriodShown : Bool
    }


init : Settings -> ( Model, Cmd Msg )
init settings =
    ( { settings = settings
      , mode = Hours
      , hoursSelected = settings.hours
      , minutesSelected = settings.minutes
      , isSelecting = False
      }
    , Cmd.none
    )


calculateModel : Model -> CalculatedModel
calculateModel model =
    { outerNumbers = outerNumbersToDisplay model.mode model.settings.is24Hours
    , innerNumbers = innerNumbersToDisplay model.mode model.settings.is24Hours
    , digitalTimeHours = digitalTimeHoursToDisplay model.settings.is24Hours model.hoursSelected
    , digitalTimeMinutes = doubleDigitFormat model.minutesSelected
    , digitalTimeSelected = Hours
    , timePeriodSelected = AM
    , timePeriodShown = not model.settings.is24Hours
    }


digitalTimeHoursToDisplay : Bool -> Int -> String
digitalTimeHoursToDisplay is24Hours hours =
    if is24Hours then
        doubleDigitFormat hours
    else
        doubleDigitFormat <| toAmHours hours


outerNumbersToDisplay : Mode -> Bool -> List String
outerNumbersToDisplay mode is24Hours =
    case mode of
        Hours ->
            List.map toDoubleZeroString <| 12 :: (List.range 1 11)

        Minutes ->
            List.map toDoubleZeroString <| List.map (\x -> x * 5) <| List.range 0 11


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


type Msg
    = Noop
    | ModeSwitch Mode
    | TimePeriodSwitch TimePeriod
    | MouseDown Offset
    | MouseMove Offset
    | MouseUp Offset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        MouseDown offset ->
            let
                modelWithSelection =
                    applySelection model <|
                        offsetToSelection offset (modeToStep model.mode)
            in
                ( { modelWithSelection | isSelecting = True }, Cmd.none )

        MouseMove offset ->
            if model.isSelecting == True then
                let
                    selection =
                        offsetToSelection offset (modeToStep model.mode)
                in
                    ( applySelection model selection, Cmd.none )
            else
                ( model, Cmd.none )

        MouseUp offset ->
            case model.mode of
                Hours ->
                    ( { model
                        | isSelecting = False
                        , mode = Minutes
                      }
                    , Cmd.none
                    )

                Minutes ->
                    ( { model | isSelecting = False }, Cmd.none )

        ModeSwitch mode ->
            ( { model
                | mode = mode
              }
            , Cmd.none
            )

        TimePeriodSwitch timePeriod ->
            case timePeriod of
                AM ->
                    ( { model | hoursSelected = toAmHours model.hoursSelected }, Cmd.none )

                PM ->
                    ( { model | hoursSelected = toPmHours model.hoursSelected }, Cmd.none )


applySelection : Model -> Selection -> Model
applySelection model selection =
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
                        { model | minutesSelected = selectedMinute }

                    Nothing ->
                        model

        Hours ->
            case model.settings.is24Hours of
                True ->
                    applySelection24h model selection

                False ->
                    applySelection12h model selection


applySelection24h : Model -> Selection -> Model
applySelection24h model selection =
    case selection.isInner of
        True ->
            let
                maybeHour =
                    List.head (List.drop selection.angle hoursInner)
            in
                case maybeHour of
                    Just selectedHour ->
                        { model | hoursSelected = selectedHour }

                    Nothing ->
                        model

        False ->
            let
                maybeHour =
                    List.head (List.drop selection.angle hoursOuter)
            in
                case maybeHour of
                    Just selectedHour ->
                        { model | hoursSelected = selectedHour }

                    Nothing ->
                        model


applySelection12h : Model -> Selection -> Model
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
                        { model | hoursSelected = adjustedHour }
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
                        { model | hoursSelected = adjustedPmHour }

            Nothing ->
                model


center : Position
center =
    { x = 130
    , y = 140
    }


innerDistance : Int
innerDistance =
    90


offsetToSelection : Offset -> Int -> Selection
offsetToSelection offset step =
    let
        angle =
            angleFromOffset offset

        dist =
            distance (Position (toFloat offset.x) (toFloat offset.y)) center
    in
        { angle = angleWithStep angle step
        , isInner = dist <= innerDistance
        }


distance : Position -> Position -> Int
distance position1 position2 =
    let
        deltaX =
            position1.x - position2.x

        deltaY =
            position1.y - position2.y
    in
        round <| sqrt <| (deltaX * deltaX) + (deltaY * deltaY)


angleFromOffset : Offset -> Float
angleFromOffset offset =
    let
        x =
            offset.x - (round center.x)

        y =
            -(offset.y - (round center.y))

        test =
            ( x, y )

        angleRad =
            atan2 (toFloat y) (toFloat x)
    in
        adjustedDegrees <| toDegrees angleRad


adjustedDegrees : Float -> Float
adjustedDegrees value =
    let
        adjustedValue =
            if value >= 0 && value <= 90 then
                90 - value
            else if value >= -90 && value <= 0 then
                90 + (-value)
            else if value >= -190 && value < -90 then
                180 + (-value - 90)
            else
                270 + (180 - value)
    in
        adjustedValue


angleWithStep : Float -> Int -> Int
angleWithStep rawDegrees step =
    let
        roundedDegrees =
            roundWithStep rawDegrees step

        angle =
            floor (toFloat roundedDegrees / (toFloat step))
    in
        if angle == floor (360 / (toFloat step)) then
            0
        else
            angle


roundWithStep : Float -> Int -> Int
roundWithStep value step =
    round (value / (toFloat step)) * step


toDegrees : Float -> Float
toDegrees radians =
    radians * (180 / pi)


outerNumbers : List Int
outerNumbers =
    [ 12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]


doubleDigitFormat : Int -> String
doubleDigitFormat number =
    if number < 10 then
        "0" ++ toString number
    else
        toString number


numbersDisplay : Mode -> Int -> Int -> Html Msg
numbersDisplay mode hours minutes =
    div [ Html.Attributes.class "time-display-numbers" ]
        [ (numberDisplay (mode == Hours) hours Hours)
        , span [] [ text ":" ]
        , (numberDisplay (mode == Minutes) minutes Minutes)
        ]


numberDisplay : Bool -> Int -> Mode -> Html Msg
numberDisplay isActive value otherMode =
    case isActive of
        True ->
            span
                [ Html.Attributes.class "number"
                , onClick <| ModeSwitch otherMode
                ]
                [ text <| doubleDigitFormat value ]

        False ->
            span
                [ Html.Attributes.class "number"
                , Html.Attributes.style [ ( "opacity", "0.7" ) ]
                , onClick <| ModeSwitch otherMode
                ]
                [ text <| doubleDigitFormat value ]


timeDisplay24h : Mode -> Int -> Int -> Html Msg
timeDisplay24h mode hours minutes =
    div [ Html.Attributes.class "time-display-numbers-container" ]
        [ div [ Html.Attributes.class "side-filler" ] []
        , (numbersDisplay mode hours minutes)
        , div [ Html.Attributes.class "side-filler" ] []
        ]


timeDisplay12h : Mode -> Int -> Int -> Html Msg
timeDisplay12h mode hours minutes =
    div [ Html.Attributes.class "time-display-numbers-container" ]
        [ div [ Html.Attributes.class "side-filler" ] []
        , (numbersDisplay mode (toAmHours hours) minutes)
        , div [ Html.Attributes.class "time-periods" ]
            [ div
                [ Html.Attributes.class <| periodClass PM hours
                , onClick <| TimePeriodSwitch PM
                ]
                [ text "PM" ]
            , div
                [ Html.Attributes.class <| periodClass AM hours
                , onClick <| TimePeriodSwitch AM
                ]
                [ text "AM" ]
            ]
        ]


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


periodClass : TimePeriod -> Int -> String
periodClass timePeriod hours =
    case timePeriod of
        AM ->
            if isAm hours then
                "am"
            else
                "am inactive"

        PM ->
            if isAm hours then
                "pm inactive"
            else
                "pm"


isAm : Int -> Bool
isAm hours =
    if hours >= 0 && hours < 12 then
        True
    else
        False


timeDisplay : Bool -> Mode -> Int -> Int -> Html Msg
timeDisplay is24Hours =
    case is24Hours of
        True ->
            timeDisplay24h

        False ->
            timeDisplay12h


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "time-picker" ]
        [ div [ Html.Attributes.class "time-display" ]
            [ timeDisplay model.settings.is24Hours model.mode model.hoursSelected model.minutesSelected
            ]
        , div [ Html.Attributes.class "time-picker-clock-face" ]
            [ div [ Html.Attributes.class "clock-face-background" ] []
            , div
                [ Html.Attributes.class "numbers-container"
                , onMouseMove MouseMove
                , onMouseDown MouseDown
                , onMouseUp MouseUp
                ]
                ((clockFace model.settings.is24Hours model.mode model.hoursSelected model.minutesSelected)
                    ++ [ div [ Html.Attributes.class "filler" ] [] ]
                )
            ]
        ]


clockFace : Bool -> Mode -> Int -> Int -> List (Html Msg)
clockFace is24Hours mode hours minutes =
    case mode of
        Hours ->
            case is24Hours of
                True ->
                    hoursView24h hours

                False ->
                    hoursView12h hours

        Minutes ->
            minutesView minutes


minutesView : Int -> List (Html Msg)
minutesView selected =
    pointerView False (pointerAngle60 selected) :: (outerNumbersView minutes selected)


hoursView12h : Int -> List (Html Msg)
hoursView12h selected =
    pointerView False (pointerAngle12 selected) :: (outerNumbersView hoursOuter (toAmHours selected))


hoursView24h : Int -> List (Html Msg)
hoursView24h selected =
    (pointerView (isInnerSelected selected) (pointerAngle12 selected))
        :: ((outerNumbersView hoursOuter selected) ++ (innerNumbersView hoursInner selected))


isInnerSelected : Int -> Bool
isInnerSelected selected =
    List.member selected hoursInner


pointerAngle12 : Int -> Float
pointerAngle12 =
    pointerAngle 12


pointerAngle60 : Int -> Float
pointerAngle60 =
    pointerAngle 60


pointerAngle : Int -> Int -> Float
pointerAngle base value =
    (360 / (toFloat base)) * (toFloat value)


pointerView : Bool -> Float -> Html Msg
pointerView isInner angle =
    let
        height =
            if isInner then
                "27"
            else
                "40"
    in
        div
            [ Html.Attributes.class "arrow-contaner"
            , Html.Attributes.style
                [ ( "height", height ++ "%" )
                , ( "transform", "rotateZ(" ++ toString angle ++ "deg)" )
                ]
            ]
            [ div [ Html.Attributes.class "arrow" ] [] ]


outerNumbersView : List Int -> Int -> List (Html Msg)
outerNumbersView numbers selectedNumber =
    List.map2 (numberViewWrapper selectedNumber "number-outer") numbers outerPositions


innerNumbersView : List Int -> Int -> List (Html Msg)
innerNumbersView numbers selectedNumber =
    List.map2 (numberViewWrapper selectedNumber "number-inner") numbers innerPositions


numberViewWrapper : Int -> String -> Int -> Position -> Html Msg
numberViewWrapper selectedNumber spanClass number position =
    if selectedNumber == number then
        numberView (spanClass ++ " number-selected") number position
    else if selectedNumber == 0 && number == 12 then
        numberView (spanClass ++ " number-selected") number position
    else
        numberView spanClass number position


numberView : String -> Int -> Position -> Html Msg
numberView spanClass number position =
    span
        [ Html.Attributes.class spanClass
        , Html.Attributes.style [ ( "transform", "translate(" ++ (toString position.x) ++ "px, " ++ (toString position.y) ++ "px)" ) ]
        ]
        [ text <| toString number ]
