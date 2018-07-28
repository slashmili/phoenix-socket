module Phoenix.ChannelHelper exposing (onJoinedCommand, onFailedToJoinCommand, onCustomCommand)

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg(..))
import Json.Decode as Decode
import Task
import Dict


onReceiveMsg : String -> Decode.Value -> Channel msg -> Msg msg
onReceiveMsg name response channel =
    case Dict.get name channel.receive of
        Just cmd ->
            ExternalMsg (cmd response)
        _ ->
            Message.none


onJoinedCommand : Decode.Value -> Channel msg -> Msg msg
onJoinedCommand response channel =
    onReceiveMsg "ok" response channel

onFailedToJoinCommand : Decode.Value -> Channel msg -> Msg msg
onFailedToJoinCommand response channel =
    onReceiveMsg "error" response channel


onCustomCommand : String -> Decode.Value -> Channel msg -> Msg msg
onCustomCommand eventName response channel =
    case Dict.get eventName channel.on of
        Just cmd ->
            ExternalMsg (cmd response)

        _ ->
            Message.none
