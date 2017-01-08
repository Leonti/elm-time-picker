module TimePicker exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import MousePosition exposing (..)
import TimeTypes exposing (..)
import CalculatedModel exposing (InputModel, CalculatedModel, SelectedTime, toSelectedTime, calculateModel, toAmHours, toPmHours)


type alias Position =
    { x : Float
    , y : Float
    }


type alias Settings =
    { is24Hours : Bool
    , hours : Int
    , minutes : Int
    }


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
    let
        selectedTime =
            toSelectedTime
                { is24Hours = model.settings.is24Hours
                , mode = model.mode
                , hoursSelected = model.hoursSelected
                , minutesSelected = model.minutesSelected
                }
                selection
    in
        { model
            | hoursSelected = selectedTime.hoursSelected
            , minutesSelected = selectedTime.minutesSelected
        }


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


digitalNumbersDisplay : Mode -> String -> String -> Html Msg
digitalNumbersDisplay mode hours minutes =
    div [ Html.Attributes.class "time-display-numbers" ]
        [ (digitalNumberDisplay (mode == Hours) hours Hours)
        , span [] [ text ":" ]
        , (digitalNumberDisplay (mode == Minutes) minutes Minutes)
        ]


digitalNumberDisplay : Bool -> String -> Mode -> Html Msg
digitalNumberDisplay isActive value otherMode =
    case isActive of
        True ->
            span
                [ Html.Attributes.class "number"
                , onClick <| ModeSwitch otherMode
                ]
                [ text value ]

        False ->
            span
                [ Html.Attributes.class "number"
                , Html.Attributes.style [ ( "opacity", "0.7" ) ]
                , onClick <| ModeSwitch otherMode
                ]
                [ text <| value ]


timeDisplay24h : Mode -> String -> String -> Html Msg
timeDisplay24h mode hours minutes =
    div [ Html.Attributes.class "time-display-numbers-container" ]
        [ div [ Html.Attributes.class "side-filler" ] []
        , (digitalNumbersDisplay mode hours minutes)
        , div [ Html.Attributes.class "side-filler" ] []
        ]


timeDisplay12h : Mode -> TimePeriod -> String -> String -> Html Msg
timeDisplay12h mode timePeriod hours minutes =
    div [ Html.Attributes.class "time-display-numbers-container" ]
        [ div [ Html.Attributes.class "side-filler" ] []
        , (digitalNumbersDisplay mode hours minutes)
        , div [ Html.Attributes.class "time-periods" ]
            [ div
                [ Html.Attributes.class <| periodClass PM timePeriod
                , onClick <| TimePeriodSwitch PM
                ]
                [ text "PM" ]
            , div
                [ Html.Attributes.class <| periodClass AM timePeriod
                , onClick <| TimePeriodSwitch AM
                ]
                [ text "AM" ]
            ]
        ]


periodClass : TimePeriod -> TimePeriod -> String
periodClass timePeriod selectedTimePeriod =
    case timePeriod of
        AM ->
            if selectedTimePeriod == AM then
                "am"
            else
                "am inactive"

        PM ->
            if selectedTimePeriod == AM then
                "pm inactive"
            else
                "pm"


digitalTimeView : CalculatedModel -> Html Msg
digitalTimeView cm =
    case cm.timePeriodShown of
        True ->
            timeDisplay12h cm.mode cm.timePeriodSelected cm.digitalTimeHours cm.digitalTimeMinutes

        False ->
            timeDisplay24h cm.mode cm.digitalTimeHours cm.digitalTimeMinutes


view : Model -> Html Msg
view model =
    let
        calculatedModel =
            calculateModel
                { is24Hours = model.settings.is24Hours
                , mode = model.mode
                , hoursSelected = model.hoursSelected
                , minutesSelected = model.minutesSelected
                }
    in
        div [ Html.Attributes.class "time-picker" ]
            [ div [ Html.Attributes.class "time-display" ]
                [ digitalTimeView calculatedModel
                ]
            , clockFaceView calculatedModel
            ]


clockFaceView : CalculatedModel -> Html Msg
clockFaceView cm =
    let
        outerNumbersView =
            List.map2 (numberViewWrapper cm.selectedNumber "number-outer") cm.outerNumbers outerPositions

        pointerViewToRender =
            pointerView cm.isShortPointer cm.pointerAngle
    in
        div [ Html.Attributes.class "time-picker-clock-face" ]
            [ div [ Html.Attributes.class "clock-face-background" ] []
            , div
                [ Html.Attributes.class "numbers-container"
                , onMouseMove MouseMove
                , onMouseDown MouseDown
                , onMouseUp MouseUp
                ]
                ((pointerViewToRender :: outerNumbersView)
                    ++ [ div [ Html.Attributes.class "filler" ] [] ]
                )
            ]


pointerView : Bool -> Int -> Html Msg
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


numberViewWrapper : String -> String -> String -> Position -> Html Msg
numberViewWrapper selectedNumber spanClass number position =
    if selectedNumber == number then
        numberView (spanClass ++ " number-selected") number position
    else
        numberView spanClass number position


numberView : String -> String -> Position -> Html Msg
numberView spanClass number position =
    span
        [ Html.Attributes.class spanClass
        , Html.Attributes.style [ ( "transform", "translate(" ++ (toString position.x) ++ "px, " ++ (toString position.y) ++ "px)" ) ]
        ]
        [ text number ]
