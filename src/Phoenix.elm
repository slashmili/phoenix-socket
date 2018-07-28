module Phoenix exposing (connect, update, join)

{-|
# Basic Usage

@docs connect, update, join
-}

import Phoenix.Socket as Socket exposing(Socket)
import Phoenix.Channel exposing(Channel)
import Phoenix.Message as Message exposing(Msg)

{-|
What t?
-}
connect : Socket msg -> (Msg msg -> msg) -> Sub msg
connect socket toExternalAppMsgFn =
    Socket.listen socket toExternalAppMsgFn


{-|
update
-}
update: (Msg msg -> msg) -> Msg msg -> Socket msg -> (Socket msg, Cmd msg)
update toExternalAppMsgFn msg socket =
    Socket.update toExternalAppMsgFn msg socket

{-|
join
-}
join: Channel msg -> Socket msg -> (Socket msg, Cmd (Msg msg))
join channel socket =
    Socket.join channel socket
