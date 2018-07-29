module Phoenix.Push exposing (Push, init, withPayload)

import Phoenix.Channel exposing (Channel)
import Json.Encode as Encode


type alias Push msg =
    { event : String
    , channel : Channel msg
    , payload : Encode.Value
    }


init : String -> Channel msg -> Push msg
init event channel =
    { event = event, channel = channel, payload = Encode.object [] }


withPayload : Encode.Value -> Push msg -> Push msg
withPayload payload push =
    { push | payload = payload }
