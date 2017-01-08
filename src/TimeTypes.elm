module TimeTypes exposing (..)


type Mode
    = Hours
    | Minutes


type TimePeriod
    = AM
    | PM


type alias Selection =
    { angle : Int
    , isInner : Bool
    }
