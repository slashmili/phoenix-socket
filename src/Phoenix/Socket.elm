module Phoenix.Socket exposing (init, Socket, listen, withChannel)

{-|
# Basic Usage

@docs init, Socket, listen

@docs withChannel

-}

import Phoenix.Channel exposing (Channel)
import Dict exposing (Dict)
import WebSocket


type Transport
    = WebSocket
    | LongPoll

{-| Socket options
-}
type alias Socket msg =
    { endPoint : String
    , transport: Transport
    , channels: Dict String (Channel msg)
    }


{-| initialize a Socket

-}
init : String -> Socket msg
init endPoint =
    { endPoint = endPoint
    , transport = WebSocket
    , channels = Dict.empty
    }

{-| What ever
-}
listen : Socket msg -> (String -> msg) -> Sub msg
listen socket fn=
    WebSocket.listen socket.endPoint fn

{-| Whart ever
What do you want from me?
-}
withChannel: Channel msg -> Socket msg -> Socket msg
withChannel channel socket =
    {socket | channels = Dict.insert channel.topic channel socket.channels}
