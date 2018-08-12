module Phoenix.Internal.WebSocket exposing (..)

import Dict exposing (Dict)
import Json.Encode as Encode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.Channel as ChannelHelper
import Phoenix.Internal.Socket as SocketHelper
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Push as Push exposing (Push)
import WebSocket as NativeWebSocket


listen : String -> Dict.Dict String (Channel msg) -> Dict Int (Push msg) -> Float -> (Msg msg -> msg) -> Sub msg
listen endPoint channels pushedEvents heartbeatIntervalSeconds toExternalAppMsgFn =
    let
        mappedMsg =
            Message.mapAll toExternalAppMsgFn

        subs =
            Sub.batch
                [ internalMsgs endPoint channels
                , externalMsgs endPoint pushedEvents channels
                , heartbeatSubscription heartbeatIntervalSeconds
                ]
    in
    Sub.map mappedMsg subs


internalMsgs : String -> Dict.Dict String (Channel msg) -> Sub (Msg msg)
internalMsgs endPoint channels =
    Sub.map (SocketHelper.mapMaybeInternalEvents channels) (phoenixMessages endPoint)


externalMsgs : String -> Dict Int (Push msg) -> Dict.Dict String (Channel msg) -> Sub (Msg msg)
externalMsgs endPoint pushedEvents channels =
    Sub.map (SocketHelper.mapMaybeExternalEvents pushedEvents channels) (phoenixMessages endPoint)


heartbeatSubscription : Float -> Sub (Msg msg)
heartbeatSubscription heartbeatIntervalSeconds =
    SocketHelper.heartbeatSubscription heartbeatIntervalSeconds


phoenixMessages : String -> Sub (Maybe Event)
phoenixMessages endPoint =
    NativeWebSocket.listen endPoint Event.decode


send : String -> Event -> Cmd (Msg msg)
send path event =
    NativeWebSocket.send path (Event.encode event)
