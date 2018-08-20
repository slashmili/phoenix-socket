module Phoenix.Socket exposing (Socket, channels, endPoint, heartbeatTimestamp, init, join, listen, push, pushedEvents, subscribe, update, withLongPoll)

{-|


# This module provides an interface for connecting to Phoenix Socket

@docs Socket, init, update, join, listen, push, withLongPoll, subscribe, endPoint, pushedEvents, channels, heartbeatTimestamp

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
type Socket msg
    = Socket
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
    Socket
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
withLongPoll (Socket socket) =
    Socket
        { socket
            | transport = LongPoll
        }


{-| Joins a channel
Adds the channel to the socket model and returns a command that sends data
-}
join : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
join channel (Socket socket) =
    case Dict.get (Channel.topic channel) socket.channels of
        Just channelItm ->
            if Channel.isOngoing channelItm then
                ( Socket socket, Cmd.none )

            else
                doJoin channel (Socket socket)

        Nothing ->
            doJoin channel (Socket socket)


{-| Subscribes to a channel events
Adds the channel to the socket model
-}
subscribe : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
subscribe channel socket =
    ( addChannel (Channel.setJoinedState channel) socket, Cmd.none )


{-| Listens to socket
-}
listen : (Msg msg -> msg) -> Socket msg -> Sub msg
listen toExternalAppMsgFn (Socket socket) =
    InternalWebSocket.listen socket.endPoint socket.channels socket.pushedEvents socket.heartbeatIntervalSeconds toExternalAppMsgFn


{-| Handles Phoenix Msg
-}
update : (Msg msg -> msg) -> Msg msg -> Socket msg -> ( Socket msg, Cmd msg )
update toExternalAppMsgFn msg (Socket socket) =
    case Message.extractInternalMsg msg of
        ChannelSuccessfullyJoined channel response ->
            let
                updatedChannel =
                    Channel.setJoinedState channel

                updateSocket =
                    Socket { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        ChannelFailedToJoin channel response ->
            let
                updatedChannel =
                    Channel.setErroredState channel

                updateSocket =
                    Socket { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        ChannelError channel response ->
            let
                updatedChannel =
                    Channel.setErroredState channel

                updateSocket =
                    Socket { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        ChannelClosed channel response ->
            let
                updatedChannel =
                    Channel.setClosedState channel

                updateSocket =
                    Socket { socket | channels = Channel.updateChannel updatedChannel socket.channels }
            in
            ( updateSocket, Cmd.none )

        Heartbeat heartbeatTimestamp ->
            let
                ( updateSocket, cmd ) =
                    heartbeat (Socket { socket | heartbeatTimestamp = heartbeatTimestamp })
            in
            ( updateSocket, Cmd.map toExternalAppMsgFn cmd )

        HeartbeatReply ->
            ( Socket { socket | heartbeatReplyTimestamp = socket.heartbeatTimestamp }, Cmd.none )

        _ ->
            ( Socket socket, Cmd.none )


{-| pushs a message
-}
push : Push msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
push pushRecord (Socket socket) =
    let
        event =
            Event pushRecord.event pushRecord.topic pushRecord.payload (Just socket.ref)
    in
    doPush event (Just pushRecord) (Socket socket)


doPush : Event -> Maybe (Push msg) -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
doPush event maybePush (Socket socket) =
    let
        socketType =
            addPushedEvent maybePush (Socket socket)
    in
    ( socketType, InternalWebSocket.send socket.endPoint event )


doJoin : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
doJoin channel (Socket socket) =
    let
        eventName =
            "phx_join"

        updatedChannel =
            Channel.setJoiningState socket.ref channel

        event =
            Event.init eventName (Channel.topic channel) (Channel.payload channel) (Just socket.ref)

        updateSocket =
            Socket socket
                |> addPushedEvent Nothing
                |> addChannel updatedChannel
    in
    -- LongPoll ->
    --        (updateSocket, LongPoll.send socket.endPoint socket.longPollToken event)
    ( updateSocket, InternalWebSocket.send socket.endPoint event )


addPushedEvent : Maybe (Push msg) -> Socket msg -> Socket msg
addPushedEvent maybePush (Socket socket) =
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
    Socket { socket | pushedEvents = pushedEvents, ref = socket.ref + 1 }


addChannel : Channel msg -> Socket msg -> Socket msg
addChannel channel (Socket socket) =
    Socket { socket | channels = Channel.addChannel channel socket.channels }


heartbeat : Socket msg -> ( Socket msg, Cmd (Msg msg) )
heartbeat (Socket socket) =
    let
        event =
            Event.init "heartbeat" "phoenix" (Encode.object []) (Just socket.ref)
    in
    doPush event Nothing (Socket socket)


{-| -}
endPoint : Socket msg -> String
endPoint (Socket { endPoint }) =
    endPoint


{-| -}
pushedEvents : Socket msg -> Dict Int (Push msg)
pushedEvents (Socket { pushedEvents }) =
    pushedEvents


{-| -}
channels : Socket msg -> Dict String (Channel msg)
channels (Socket { channels }) =
    channels


{-| -}
heartbeatTimestamp : Socket msg -> Float
heartbeatTimestamp (Socket { heartbeatTimestamp }) =
    heartbeatTimestamp
