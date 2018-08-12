module Phoenix.Socket exposing (Socket, init, join, listen, push, update, withLongPoll)

{-|


# This module provides an interface for connecting to Phoenix Socket

@docs Socket, init, update, join, listen, push, withLongPoll

-}

import Dict exposing (Dict)
import Json.Encode as Encode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Phoenix.Internal.WebSocket as InternalWebSocket
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Push as Push exposing (Push)
import Phoenix.Serializer exposing (Serializer(..))


type Transport
    = WebSocket
    | LongPoll


type State
    = Connecting
    | Open
    | Closing
    | Closed


{-| Socket model
-}
type alias Socket msg =
    { endPoint : String
    , channels : Dict String (Channel msg)
    , serializer : Serializer
    , transport : Transport
    , pushedEvents : Dict Int (Push msg)
    , heartbeatIntervalSeconds : Float
    , heartbeatTimestamp : Float
    , heartbeatReplyTimestamp : Float
    , longPollToken : Maybe.Maybe String
    , ref : Int
    , readyState : State
    , withDebug : Bool
    , withoutHeartbeat : Bool
    }


{-| Initializes Socket using the websocket address
-}
init : String -> Socket msg
init endPoint =
    { endPoint = endPoint
    , channels = Dict.empty
    , serializer = V1
    , transport = WebSocket
    , pushedEvents = Dict.empty
    , heartbeatIntervalSeconds = 30
    , heartbeatTimestamp = 0
    , heartbeatReplyTimestamp = 0
    , ref = 1
    , longPollToken = Nothing
    , readyState = Closed
    , withDebug = False
    , withoutHeartbeat = False
    }


{-| withLongPoll
-}
withLongPoll : Socket msg -> Socket msg
withLongPoll socket =
    { socket
        | transport = LongPoll
    }


{-| Joins a channel
Adds the channel to the socket model and returns a command that sends data
-}
join : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
join channel socket =
    case Dict.get channel.topic socket.channels of
        Just channelItm ->
            if Channel.isOngoing channelItm then
                ( socket, Cmd.none )

            else
                doJoin channel socket

        Nothing ->
            doJoin channel socket


{-| Listens to socket
-}
listen : (Msg msg -> msg) -> Socket msg -> Sub msg
listen toExternalAppMsgFn socket =
    InternalWebSocket.listen socket.endPoint socket.channels socket.pushedEvents socket.heartbeatIntervalSeconds toExternalAppMsgFn


{-| Handles Phoenix Msg
-}
update : (Msg msg -> msg) -> Msg msg -> Socket msg -> ( Socket msg, Cmd msg )
update toExternalAppMsgFn msg socket =
    case Message.extractInternalMsg msg of
        ChannelSuccessfullyJoined channel response ->
            let
                updatedChannel =
                    Channel.setJoinedState channel

                updateSocket =
                    { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        ChannelFailedToJoin channel response ->
            let
                updatedChannel =
                    Channel.setErroredState channel

                updateSocket =
                    { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        ChannelError channel response ->
            let
                updatedChannel =
                    Channel.setErroredState channel

                updateSocket =
                    { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        ChannelClosed channel response ->
            let
                updatedChannel =
                    Channel.setClosedState channel

                updateSocket =
                    { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        Heartbeat heartbeatTimestamp ->
            let
                ( updateSocket, cmd ) =
                    heartbeat socket
            in
            ( { updateSocket | heartbeatTimestamp = heartbeatTimestamp }, Cmd.map toExternalAppMsgFn cmd )

        HeartbeatReply ->
            ( { socket | heartbeatReplyTimestamp = socket.heartbeatTimestamp }, Cmd.none )

        _ ->
            ( socket, Cmd.none )


{-| pushs a message
-}
push : Push msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
push pushRecord socket =
    let
        event =
            Event pushRecord.event pushRecord.topic pushRecord.payload (Just socket.ref)
    in
    doPush event (Just pushRecord) socket


doPush : Event -> Maybe (Push msg) -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
doPush event maybePush socket =
    let
        updateSocket =
            addPushedEvent maybePush socket
    in
    ( updateSocket, InternalWebSocket.send socket.endPoint event )


doJoin : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
doJoin channel socket =
    let
        eventName =
            "phx_join"

        updatedChannel =
            Channel.setJoiningState socket.ref channel

        event =
            Event.init eventName channel.topic channel.payload (Just socket.ref)

        updateSocket =
            socket
                |> addPushedEvent Nothing
                |> addChannel updatedChannel
    in
    -- LongPoll ->
    --        (updateSocket, LongPoll.send socket.endPoint socket.longPollToken event)
    ( updateSocket, InternalWebSocket.send socket.endPoint event )


addPushedEvent : Maybe (Push msg) -> Socket msg -> Socket msg
addPushedEvent maybePush socket =
    let
        pushedEvents =
            case maybePush of
                Just push ->
                    case Dict.size push.on of
                        0 ->
                            socket.pushedEvents

                        _ ->
                            Dict.insert socket.ref push socket.pushedEvents

                Nothing ->
                    socket.pushedEvents
    in
    { socket | pushedEvents = pushedEvents, ref = socket.ref + 1 }


addChannel : Channel msg -> Socket msg -> Socket msg
addChannel channel socket =
    { socket | channels = Channel.addChannel channel socket.channels }


heartbeat : Socket msg -> ( Socket msg, Cmd (Msg msg) )
heartbeat socket =
    let
        event =
            Event.init "heartbeat" "phoenix" (Encode.object []) (Just socket.ref)
    in
    doPush event Nothing socket
