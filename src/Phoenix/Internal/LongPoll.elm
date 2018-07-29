module Phoenix.Internal.LongPoll exposing (..)

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.ChannelHelper as ChannelHelper
import Phoenix.Message as Message exposing (Msg(..))
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.LongPollEvent exposing (longPolldecoder)
import Phoenix.Internal.SocketHelper as SocketHelper
import Task
import Json.Encode as Encode
import Dict
import Http


type alias LongPollResponse =
    {
        status: Int
        , token: String
    }


send : String -> Maybe String -> Event -> Cmd (Msg msg)
send endPoint maybeToken event =
    let
        qparam = case maybeToken of
            Just token ->
                "?token=" ++ token
            _ ->
                ""
    in

    longPolldecoder
    |> Http.post (endPoint ++ qparam) (Http.jsonBody (Event.encoder event))
    |> Http.send LongPollSent

poll: String -> Maybe String -> Cmd (Msg msg)
poll endPoint maybeToken =
    let
        qparam = case maybeToken of
            Just token ->
                "?token=" ++ token
            _ ->
                ""
    in
       Http.get (endPoint ++ qparam) longPolldecoder
       |> Http.send LongPollPolled




externalMsgs : Dict.Dict String (Channel msg) -> (Msg msg -> msg) -> List Event -> Cmd msg
externalMsgs channels toExternalAppMsgFn events =
    let
        mappedMsg =
            Message.mapAll toExternalAppMsgFn
        msgs = events
            |> List.map  (\e -> SocketHelper.mapExternalEvents channels e)
    in
       msgs
       |> List.map (\m -> m |> Task.succeed |> Task.perform identity)
       |> Cmd.batch
       |> Cmd.map mappedMsg
