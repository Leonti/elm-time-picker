module Main exposing (..)

import Html exposing (beginnerProgram, div, button, text)
import TimePicker


type alias Model =
    { timePickerModel : TimePicker.Model
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        ( timePickerModel, timePickerCmd ) =
            TimePicker.init
                { is24Hours = False
                , hours = 12
                , minutes = 25
                }
    in
        ( { timePickerModel =
                timePickerModel
          }
        , Cmd.map TimePickerMsg timePickerCmd
        )


view : Model -> Html.Html Msg
view model =
    div []
        [ div [] []
        , Html.map TimePickerMsg (TimePicker.view model.timePickerModel)
        ]


type Msg
    = TimePickerMsg TimePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePickerMsg message ->
            let
                ( timePickerModel, timePickerCmd ) =
                    TimePicker.update message model.timePickerModel
            in
                ( { model
                    | timePickerModel = timePickerModel
                  }
                , Cmd.map TimePickerMsg timePickerCmd
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [--    Sub.map TimePickerMsg TimePicker.subscriptions
        ]
