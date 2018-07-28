module Phoenix.ChannelHelper exposing (onJoinedCommand)

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg(..))
import Json.Decode as Decode
import Task
import Dict


onJoinedCommand : Decode.Value -> Channel msg -> Cmd msg
onJoinedCommand response channel =
    case Dict.get "ok" channel.receive of
        Just cmd ->
                let
                   _ = Debug.log "cmd" cmd
                in
                cmd response
                |> Task.succeed
                |> Task.perform identity
        _ ->
            Cmd.none
