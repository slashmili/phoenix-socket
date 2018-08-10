module Phoenix.Internal.Message exposing (InternalMessage(..), channelSuccessfullyJoined, channelFailedToJoin, none, channelClosed, channelError)

import Json.Decode as Decode
import Http
import Time exposing (Time)
import Phoenix.Channel exposing (Channel)
import Phoenix.Event exposing (Event)
import Phoenix.Internal.LongPollEvent exposing (LongPollEvent)


type InternalMessage msg
    = NoOp
    | ChannelSuccessfullyJoined (Channel msg) Decode.Value
    | ChannelFailedToJoin (Channel msg) Decode.Value
    | ChannelClosed (Channel msg) Decode.Value
    | ChannelError (Channel msg) Decode.Value
    | LongPollTick Time
    | LongPollSent (Result Http.Error LongPollEvent)
    | LongPollPolled (Result Http.Error LongPollEvent)


channelSuccessfullyJoined : Channel msg -> Decode.Value -> InternalMessage msg
channelSuccessfullyJoined channel response =
    ChannelSuccessfullyJoined channel response


channelFailedToJoin : Channel msg -> Decode.Value -> InternalMessage msg
channelFailedToJoin channel response =
    ChannelFailedToJoin channel response


none : InternalMessage msg
none =
    NoOp


channelClosed : Decode.Value -> Channel msg -> InternalMessage msg
channelClosed response channel =
    ChannelClosed channel response


channelError : Decode.Value -> Channel msg -> InternalMessage msg
channelError response channel =
    ChannelError channel response
