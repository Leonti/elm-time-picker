module Main exposing (..)

import Html exposing (beginnerProgram, div, button, text, p, Html)
import TimePicker
import Material.Dialog as Dialog
import Material.Button as Button
import Material
import Material.Options


type alias Model =
    { timePickerModel : TimePicker.Model
    , mdl : Material.Model
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.batch []
        }


init : ( Model, Cmd Msg )
init =
    ( { timePickerModel =
            TimePicker.init
                { is24Hours = False
                , hours = 12
                , minutes = 25
                , mainColor = "#00bcd4"
                }
      , mdl = Material.model
      }
    , Cmd.none
    )


dialogView : Model -> Html Msg
dialogView model =
    Dialog.view
        [ Material.Options.css "padding" "0" ]
        [ Dialog.content [ Material.Options.css "padding" "0" ]
            [ Html.map TimePickerMsg (TimePicker.view model.timePickerModel)
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Ok" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Cancel" ]
            ]
        ]


view : Model -> Html.Html Msg
view model =
    div []
        [ div [] []
        , Html.map TimePickerMsg (TimePicker.view model.timePickerModel)
        , (dialogView model)
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Dialog.openOn "click" ]
            [ text "Open dialog" ]
        ]


type Msg
    = TimePickerMsg TimePicker.Msg
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePickerMsg message ->
            ( { model
                | timePickerModel = TimePicker.update message model.timePickerModel
              }
            , Cmd.none
            )

        Mdl message ->
            Material.update Mdl message model
