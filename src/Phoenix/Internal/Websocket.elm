module Phoenix.Internal.WebSocket exposing (..)

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.ChannelHelper as ChannelHelper
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.SocketHelper as SocketHelper
import WebSocket as NativeWebSocket
import Json.Encode as Encode
import Dict


listen : String -> Dict.Dict String (Channel msg) -> (Msg msg -> msg) -> Sub msg
listen endPoint channels toExternalAppMsgFn =
    let
        mappedMsg =
            Message.mapAll toExternalAppMsgFn

        subs =
            Sub.batch
                [ internalMsgs endPoint channels
                , externalMsgs endPoint channels
                ]
    in
        Sub.map mappedMsg subs


internalMsgs : String -> Dict.Dict String (Channel msg) -> Sub (Msg msg)
internalMsgs endPoint channels =
    Sub.map (SocketHelper.mapMaybeInternalEvents channels) (phoenixMessages endPoint)


externalMsgs : String -> Dict.Dict String (Channel msg) -> Sub (Msg msg)
externalMsgs endPoint channels =
    Sub.map (SocketHelper.mapMaybeExternalEvents channels) (phoenixMessages endPoint)


phoenixMessages : String -> Sub (Maybe Event)
phoenixMessages endPoint =
    NativeWebSocket.listen endPoint Event.decode


send : String -> Event -> Cmd (Msg msg)
send path event =
    NativeWebSocket.send path (Event.encode event)
