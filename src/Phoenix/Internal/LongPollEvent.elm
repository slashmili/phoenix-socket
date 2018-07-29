module Phoenix.Internal.LongPollEvent exposing (..)


import Phoenix.Event as Event exposing(Event)
import Json.Decode as Decode exposing (Decoder, field, maybe, int, string, list, map3)

type alias LongPollEvent =
    { status : Int
    , token : Maybe String
    , messages: Maybe (List Event)
    }


longPolldecoder : Decoder LongPollEvent
longPolldecoder =
    map3 LongPollEvent
        (field "status" int)
        (maybe (field "token" (string)))
        (maybe (field "messages" (list Event.decoder)))
