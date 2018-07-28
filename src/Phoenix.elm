module Phoenix exposing (listen, update, join)

{-|
# Basic Usage

@docs listen, update, join
-}

import Phoenix.Socket as Socket exposing(Socket)
import Phoenix.Channel exposing(Channel)
import Phoenix.Message as Message exposing(Msg)

{-|
What t?
-}
listen : (Msg msg -> msg) -> Socket msg ->  Sub msg
listen toExternalAppMsgFn socket  =
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
join: (Msg msg -> msg) -> Channel msg -> Socket msg -> (Socket msg, Cmd msg)
join toExternalAppMsgFn channel socket =
    let
        (updateSocket, phxCmd) = Socket.join channel socket
    in
       (updateSocket, Cmd.map toExternalAppMsgFn phxCmd)
