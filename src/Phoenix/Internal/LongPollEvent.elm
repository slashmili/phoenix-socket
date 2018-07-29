module Phoenix.Internal.LongPollEvent exposing (..)


import Json.Decode as Decode exposing (field)

type alias LongPollEvent =
    { status : Int
    , token : Maybe String
    }


longPolldecoder : Decode.Decoder LongPollEvent
longPolldecoder =
    Decode.map2 LongPollEvent
        (field "status" Decode.int)
        (field "token" (Decode.maybe Decode.string))
