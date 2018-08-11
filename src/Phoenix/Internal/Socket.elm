module Phoenix.Internal.Socket exposing (..)

import Dict
import Json.Encode as Encode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.Channel as ChannelHelper
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Phoenix.Message as Message exposing (Msg)
import Time
import WebSocket as NativeWebSocket


mapMaybeInternalEvents : Dict.Dict String (Channel msg) -> Maybe Event -> Msg msg
mapMaybeInternalEvents channels maybeEvent =
    case maybeEvent of
        Just event ->
            case ( event.topic, event.event ) of
                ( "phoenix", "phx_reply" ) ->
                    Message.toInternalMsg HeartbeatReply

                _ ->
                    mapInternalEvents channels event

        Nothing ->
            Message.none


mapInternalEvents : Dict.Dict String (Channel msg) -> Event -> Msg msg
mapInternalEvents channels event =
    let
        channel =
            Channel.findChannel event.topic
    in
    case event.event of
        "phx_reply" ->
            handleInternalPhxReply channels event

        "phx_close" ->
            channels
                |> channel
                |> Maybe.andThen (\chan -> Just (Message.toInternalMsg (InternalMessage.channelClosed event.payload chan)))
                |> Maybe.withDefault Message.none

        "phx_error" ->
            channels
                |> channel
                |> Maybe.andThen (\chan -> Just (Message.toInternalMsg (InternalMessage.channelError event.payload chan)))
                |> Maybe.withDefault Message.none

        _ ->
            Message.none


mapMaybeExternalEvents : Dict.Dict String (Channel msg) -> Maybe Event -> Msg msg
mapMaybeExternalEvents channels maybeEvent =
    case maybeEvent of
        Just event ->
            mapExternalEvents channels event

        Nothing ->
            Message.none


mapExternalEvents : Dict.Dict String (Channel msg) -> Event -> Msg msg
mapExternalEvents channels event =
    let
        channelWithRef =
            Channel.findChannelWithRef event.topic event.ref

        channel =
            Channel.findChannel event.topic
    in
    case event.event of
        "phx_reply" ->
            case channelWithRef channels of
                Just chan ->
                    case Event.decodeReply event.payload of
                        Ok response ->
                            ChannelHelper.onJoinedCommand response chan

                        Err response ->
                            ChannelHelper.onFailedToJoinCommand response chan

                Nothing ->
                    Message.none

        "phx_error" ->
            channels
                |> channel
                |> Maybe.andThen (\chan -> Just (ChannelHelper.onErrorCommand event.payload chan))
                |> Maybe.withDefault Message.none

        "phx_close" ->
            channels
                |> channel
                |> Maybe.andThen (\chan -> Just (ChannelHelper.onClosedCommand event.payload chan))
                |> Maybe.withDefault Message.none

        _ ->
            channels
                |> channel
                |> Maybe.andThen (\chan -> Just (ChannelHelper.onCustomCommand event.event event.payload chan))
                |> Maybe.withDefault Message.none


handleInternalPhxReply : Dict.Dict String (Channel msg) -> Event -> Msg msg
handleInternalPhxReply channels event =
    case Channel.findChannelWithRef event.topic event.ref channels of
        Just channel ->
            case Event.decodeReply event.payload of
                Ok response ->
                    Message.toInternalMsg (InternalMessage.channelSuccessfullyJoined channel response)

                Err response ->
                    Message.toInternalMsg (InternalMessage.channelFailedToJoin channel response)

        Nothing ->
            Message.none


heartbeatSubscription : Float -> Sub (Msg msg)
heartbeatSubscription heartbeatIntervalSeconds =
    Heartbeat
        |> Time.every (Time.second * heartbeatIntervalSeconds)
        |> Sub.map Message.toInternalMsg
