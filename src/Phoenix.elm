module Phoenix exposing (connect, update, join)

{-|
# Basic Usage

@docs connect, update, join
-}

import Phoenix.Socket as Socket exposing(Socket)
import Phoenix.Channel exposing(Channel)
import Phoenix.Message exposing(Msg)

{-|
What t?
-}
connect : Socket msg -> (Msg msg -> msg) -> Sub msg
connect socket fn =
    Socket.listen socket fn


{-|
update
-}
update: Msg msg -> Socket msg -> (Socket msg, Cmd (Msg msg))
update msg socket =
    (socket , Cmd.none)

{-|
join
-}
join: Channel msg -> Socket msg -> (Socket msg, Cmd (Msg msg))
join channel socket =
    Socket.join channel socket
