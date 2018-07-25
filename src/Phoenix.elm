module Phoenix exposing (connect)

{-|
# Basic Usage

@docs connect
-}

import Phoenix.Socket as Socket exposing(Socket)
import Phoenix.Channel exposing(Channel)


type Msg msg
    = NoOp
    | ExternalMsg msg
{-|
What t?
-}
connect : Socket msg -> (String -> msg) -> Sub msg
connect socket fn =
    Socket.listen socket fn
