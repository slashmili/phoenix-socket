module Phoenix.Internal.Channel exposing (onClosedCommand, onCustomCommand, onErrorCommand, onFailedToJoinCommand, onJoinedCommand)

import Dict
import Json.Decode as Decode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Push exposing (Push)
import Task


onReceiveMsg : String -> Decode.Value -> Channel msg -> Msg msg
onReceiveMsg name response channel =
    case Dict.get name (Channel.receives channel) of
        Just cmd ->
            Message.toExternalMsg (cmd response)

        _ ->
            Message.none


onJoinedCommand : Decode.Value -> Channel msg -> Msg msg
onJoinedCommand response channel =
    onReceiveMsg "ok" response channel


onFailedToJoinCommand : Decode.Value -> Channel msg -> Msg msg
onFailedToJoinCommand response channel =
    onReceiveMsg "join_error" response channel


onClosedCommand : Decode.Value -> Channel msg -> Msg msg
onClosedCommand response channel =
    onReceiveMsg "close" response channel


onErrorCommand : Decode.Value -> Channel msg -> Msg msg
onErrorCommand response channel =
    onReceiveMsg "error" response channel


onCustomCommand : String -> Decode.Value -> Channel msg -> Msg msg
onCustomCommand eventName response channel =
    case Dict.get eventName (Channel.ons channel) of
        Just cmd ->
            Message.toExternalMsg (cmd response)

        _ ->
            Message.none
