module MousePosition
    exposing
        ( Offset
        , onMouseMove
        , onMouseDown
        , onMouseUp
        )

import Html
import Html.Events exposing (on)
import Json.Decode exposing (field)
import Json.Decode as Decode


type alias Offset =
    { x : Int
    , y : Int
    }


eventToOffsetDecoder : Decode.Decoder Offset
eventToOffsetDecoder =
    Decode.map2
        Offset
        (field "offsetX" Decode.int)
        (field "offsetY" Decode.int)


onMouseMove : (Offset -> msg) -> Html.Attribute msg
onMouseMove target =
    on "mousemove" (Decode.map target eventToOffsetDecoder)


onMouseDown : (Offset -> msg) -> Html.Attribute msg
onMouseDown target =
    on "mousedown" (Decode.map target eventToOffsetDecoder)


onMouseUp : (Offset -> msg) -> Html.Attribute msg
onMouseUp target =
    on "mouseup" (Decode.map target eventToOffsetDecoder)
