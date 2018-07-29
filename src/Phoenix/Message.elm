module Phoenix.Message exposing (Msg(..), mapAll, none, channelSuccessfullyJoined, channelFailedToJoin, channelClosed, channelError, cmdMap)

import Json.Decode as Decode
import Http
import Time exposing (Time)
import Phoenix.Channel exposing (Channel)
import Phoenix.Event exposing (Event)
import Phoenix.Internal.LongPollEvent exposing (LongPollEvent)

type Msg msg
    = NoOp
    | ExternalMsg msg
    | ChannelSuccessfullyJoined (Channel msg) Decode.Value
    | ChannelFailedToJoin (Channel msg) Decode.Value
    | ChannelClosed (Channel msg) Decode.Value
    | ChannelError (Channel msg) Decode.Value
    | LongPollTick Time
    | LongPollSent (Result Http.Error LongPollEvent)
    | LongPollPolled (Result Http.Error LongPollEvent)


mapAll : (Msg msg -> msg) -> Msg msg -> msg
mapAll fn internalMsg =
    case internalMsg of
        ExternalMsg msg ->
            msg

        _ ->
            fn internalMsg

cmdMap : (Msg msg -> msg) -> Cmd msg ->  Cmd msg
cmdMap fn internalMsg =
    internalMsg

none : Msg msg
none =
    NoOp


channelSuccessfullyJoined : Channel msg -> Decode.Value -> Msg msg
channelSuccessfullyJoined channel response =
    ChannelSuccessfullyJoined channel response


channelFailedToJoin : Channel msg -> Decode.Value -> Msg msg
channelFailedToJoin channel response =
    ChannelFailedToJoin channel response

channelClosed : Decode.Value -> Channel msg -> Msg msg
channelClosed response channel=
    ChannelClosed channel response

channelError : Decode.Value -> Channel msg -> Msg msg
channelError response channel=
    ChannelError channel response
