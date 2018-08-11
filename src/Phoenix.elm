module Phoenix exposing (initPushWithChannelName, join, listen, push, pushWithPayload, update)

{-|


# Basic Usage

@docs listen, update, join, push, initPushWithChannelName, pushWithPayload

-}

import Json.Encode as Encode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Push as Push exposing (Push)
import Phoenix.Socket as Socket exposing (Socket)


{-| Listens to socket change and timers
-}
listen : (Msg msg -> msg) -> Socket msg -> Sub msg
listen toExternalAppMsgFn socket =
    Socket.listen toExternalAppMsgFn socket


{-| Updates socket model and send messages
-}
update : (Msg msg -> msg) -> Msg msg -> Socket msg -> ( Socket msg, Cmd msg )
update toExternalAppMsgFn msg socket =
    Socket.update toExternalAppMsgFn msg socket


{-| Joins a channel
-}
join : (Msg msg -> msg) -> Channel msg -> Socket msg -> ( Socket msg, Cmd msg )
join toExternalAppMsgFn channel socket =
    let
        ( updateSocket, phxCmd ) =
            Socket.join channel socket
    in
    ( updateSocket, Cmd.map toExternalAppMsgFn phxCmd )


{-| Pushes a a message
-}
push : (Msg msg -> msg) -> Push msg -> Socket msg -> ( Socket msg, Cmd msg )
push toExternalAppMsgFn pushRecord socket =
    let
        ( updateSocket, phxCmd ) =
            Socket.push pushRecord socket
    in
    ( updateSocket, Cmd.map toExternalAppMsgFn phxCmd )


{-| pushWithPayload
-}
pushWithPayload : (Msg msg -> msg) -> Push msg -> Encode.Value -> Socket msg -> ( Socket msg, Cmd msg )
pushWithPayload toExternalAppMsgFn pushRecord payload socket =
    push toExternalAppMsgFn (Push.withPayload payload pushRecord) socket


{-| initPushWithChannelName
-}
initPushWithChannelName : String -> String -> Socket msg -> Maybe (Push msg)
initPushWithChannelName event channelName socket =
    socket.channels
        |> Channel.findChannel channelName
        |> Maybe.andThen (\chan -> Just (Push.init event chan))
