module Phoenix.Internal.LongPollEvent exposing (LongPollEvent, longPolldecoder)

import Json.Decode as Decode exposing (Decoder, field, int, list, map3, maybe, string)
import Phoenix.Event as Event exposing (Event)


type alias LongPollEvent =
    { status : Int
    , token : Maybe String
    , messages : Maybe (List Event)
    }


longPolldecoder : Decoder LongPollEvent
longPolldecoder =
    map3 LongPollEvent
        (field "status" int)
        (maybe (field "token" string))
        (maybe (field "messages" (list Event.decoder)))
