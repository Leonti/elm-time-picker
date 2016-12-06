module TimePicker exposing (Model, Msg, init, update, view, emptyModel)

import Html exposing (..)
import Html.Attributes exposing (..)
import MousePosition exposing (..)


--import Html.Events exposing (..)


type alias Position =
    { x : Float
    , y : Float
    }


type alias Selection =
    { angle : Int
    , isInner : Bool
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


minutes : List Int
minutes =
    List.map (\x -> x * 5) <| List.range 0 11


hoursOuter : List Int
hoursOuter =
    12 :: (List.range 1 11)


hoursInner : List Int
hoursInner =
    0 :: (List.range 13 23)


minuteStep : Int
minuteStep =
    6


hourStep : Int
hourStep =
    30


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )


type Msg
    = Noop
    | MouseDown Offset
    | MouseMove Offset
    | MouseUp Offset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        MouseDown offset ->
            ( model, Cmd.none )

        MouseMove offset ->
            let
                selection =
                    Debug.log "selection" <| offsetToSelection offset hourStep
            in
                ( model, Cmd.none )

        MouseUp offset ->
            ( model, Cmd.none )


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


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "time-picker" ]
        [ div [ Html.Attributes.class "time-display" ]
            [ div [ Html.Attributes.class "time-display-numbers-container" ]
                [ div [ Html.Attributes.class "time-display-numbers" ]
                    [ span [ Html.Attributes.class "number" ] [ text "22" ]
                    , span [] [ text ":" ]
                    , span [ Html.Attributes.class "number", Html.Attributes.style [ ( "opacity", "0.7" ) ] ] [ text "19" ]
                    ]
                ]
            ]
        , div [ Html.Attributes.class "time-picker-clock-face" ]
            [ div [ Html.Attributes.class "clock-face-background" ] []
            , div
                [ Html.Attributes.class "numbers-container"
                , onMouseMove MouseMove
                , onMouseDown MouseDown
                , onMouseUp MouseUp
                ]
                ((hoursView24h 3) ++ [ div [ Html.Attributes.class "filler" ] [] ])
            ]
        ]


minutesView : Int -> List (Html Msg)
minutesView selected =
    pointerView False (pointerAngle60 selected) :: (outerNumbersView minutes)


hoursView12h : Int -> List (Html Msg)
hoursView12h selected =
    pointerView False (pointerAngle12 selected) :: (outerNumbersView hoursOuter)


hoursView24h : Int -> List (Html Msg)
hoursView24h selected =
    (pointerView False (pointerAngle12 selected)) :: ((outerNumbersView hoursOuter) ++ (innerNumbersView hoursInner))


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
                "30"
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


outerNumbersView : List Int -> List (Html Msg)
outerNumbersView numbers =
    List.map2 (\number position -> numberView "number-outer" number position) numbers outerPositions


innerNumbersView : List Int -> List (Html Msg)
innerNumbersView numbers =
    List.map2 (\number position -> numberView "number-inner" number position) numbers innerPositions


numberView : String -> Int -> Position -> Html Msg
numberView spanClass number position =
    span
        [ Html.Attributes.class spanClass
        , Html.Attributes.style [ ( "transform", "translate(" ++ (toString position.x) ++ "px, " ++ (toString position.y) ++ "px)" ) ]
        ]
        [ text <| toString number ]
