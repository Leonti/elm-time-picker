module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import TimePicker
import Material.Dialog as Dialog
import Material.Toggles as Toggles
import Material.Button as Button
import Material
import Material.Options
import Material.Typography as Typo
import Material.Options as Options


type alias Model =
    { timePickerModel : TimePicker.Model
    , hours : Int
    , minutes : Int
    , mdl : Material.Model
    , is24Hours : Bool
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
    ( { timePickerModel = timePickerModel 13 25 True
      , hours = 13
      , minutes = 25
      , mdl = Material.model
      , is24Hours = True
      }
    , Cmd.none
    )


timePickerModel : Int -> Int -> Bool -> TimePicker.Model
timePickerModel hours minutes is24Hours =
    TimePicker.init
        { is24Hours = is24Hours
        , hours = hours
        , minutes = minutes
        , mainColor = "#00bcd4"
        }


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
                [ Dialog.closeOn "click"
                , Options.onClick TimeSelected
                ]
                [ text "Ok" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Cancel" ]
            ]
        ]


doubleDigitFormat : Int -> String
doubleDigitFormat number =
    if number < 10 then
        "0" ++ toString number
    else
        toString number


view : Model -> Html.Html Msg
view model =
    div [ Html.Attributes.style [ ( "padding", "20px" ) ] ]
        [ Options.styled p
            [ Typo.display3 ]
            [ text <| (doubleDigitFormat model.hours) ++ ":" ++ (doubleDigitFormat model.minutes) ]
        , (toggles model)
        , (dialogView model)
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Dialog.openOn "click" ]
            [ text "Change Time" ]
        , div []
            [ a [ Html.Attributes.href "https://github.com/Leonti/elm-time-picker" ] [ text "Project repo" ]
            ]
        ]


type Msg
    = TimePickerMsg TimePicker.Msg
    | Mdl (Material.Msg Msg)
    | TimeSelected
    | Toggle24h Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePickerMsg message ->
            ( { model
                | timePickerModel = TimePicker.update message model.timePickerModel
              }
            , Cmd.none
            )

        TimeSelected ->
            let
                currentTime =
                    TimePicker.selectedTime model.timePickerModel
            in
                ( { model
                    | hours = currentTime.hours
                    , minutes = currentTime.minutes
                  }
                , Cmd.none
                )

        Toggle24h is24Hours ->
            ( { model
                | timePickerModel = timePickerModel model.hours model.minutes is24Hours
                , is24Hours = is24Hours
              }
            , Cmd.none
            )

        Mdl message ->
            Material.update Mdl message model


toggles : Model -> Html Msg
toggles model =
    div
        []
        [ Toggles.radio Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value model.is24Hours
            , Toggles.group "modeGroup"
            , Toggles.ripple
            , Options.onToggle (Toggle24h True)
            ]
            [ text "24h" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value (not model.is24Hours)
            , Toggles.group "modeGroup"
            , Toggles.ripple
            , Options.onToggle (Toggle24h False)
            , Options.css "margin-left" "20px"
            ]
            [ text "AM/PM" ]
        ]
