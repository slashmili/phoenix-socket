module Phoenix.Internal.WebSocket exposing (..)

import Dict
import Json.Encode as Encode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.Channel as ChannelHelper
import Phoenix.Internal.Socket as SocketHelper
import Phoenix.Message as Message exposing (Msg)
import WebSocket as NativeWebSocket


listen : String -> Dict.Dict String (Channel msg) -> Float -> (Msg msg -> msg) -> Sub msg
listen endPoint channels heartbeatIntervalSeconds toExternalAppMsgFn =
    let
        mappedMsg =
            Message.mapAll toExternalAppMsgFn

        subs =
            Sub.batch
                [ internalMsgs endPoint channels
                , externalMsgs endPoint channels
                , heartbeatSubscription heartbeatIntervalSeconds
                ]
    in
    Sub.map mappedMsg subs


internalMsgs : String -> Dict.Dict String (Channel msg) -> Sub (Msg msg)
internalMsgs endPoint channels =
    Sub.map (SocketHelper.mapMaybeInternalEvents channels) (phoenixMessages endPoint)


externalMsgs : String -> Dict.Dict String (Channel msg) -> Sub (Msg msg)
externalMsgs endPoint channels =
    Sub.map (SocketHelper.mapMaybeExternalEvents channels) (phoenixMessages endPoint)


heartbeatSubscription : Float -> Sub (Msg msg)
heartbeatSubscription heartbeatIntervalSeconds =
    SocketHelper.heartbeatSubscription heartbeatIntervalSeconds


phoenixMessages : String -> Sub (Maybe Event)
phoenixMessages endPoint =
    NativeWebSocket.listen endPoint Event.decode


send : String -> Event -> Cmd (Msg msg)
send path event =
    NativeWebSocket.send path (Event.encode event)
