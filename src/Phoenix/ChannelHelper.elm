module Phoenix.ChannelHelper exposing (onJoinedCommand, onFailedToJoinCommand)

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg(..))
import Json.Decode as Decode
import Task
import Dict


onReceiveMsg : String -> Decode.Value -> Channel msg -> Cmd msg
onReceiveMsg name response channel =
    case Dict.get name channel.receive of
        Just cmd ->
                cmd response
                |> Task.succeed
                |> Task.perform identity
        _ ->
            Cmd.none

onJoinedCommand : Decode.Value -> Channel msg -> Cmd msg
onJoinedCommand response channel =
    onReceiveMsg "ok" response channel


onFailedToJoinCommand : Decode.Value -> Channel msg -> Cmd msg
onFailedToJoinCommand response channel =
    onReceiveMsg "error" response channel
