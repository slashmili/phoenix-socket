module Phoenix.Socket exposing (Socket, init, update, join, listen, push)

{-|
# This module provides an interface for connecting to Phoenix Socket

@docs Socket, init, update, join, listen, push
-}

import Dict exposing (Dict)
import Phoenix.Serializer exposing (Serializer(..))
import Phoenix.Internal.WebSocket as InternalWebSocket
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Push as Push exposing (Push)


type Transport
    = WebSocket
    | LongPoll


{-|
Socket model
-}
type alias Socket msg =
    { endPoint : String
    , channels : Dict String (Channel msg)
    , serializer : Serializer
    , transport : Transport
    , pushedEvents : Dict Int Event
    , ref : Int
    }


{-|
Initializes Socket using the websocket address
-}
init : String -> Socket msg
init endPoint =
    { endPoint = endPoint
    , channels = Dict.empty
    , serializer = V1
    , transport = WebSocket
    , pushedEvents = Dict.empty
    , ref = 1
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
    InternalWebSocket.listen socket.endPoint socket.channels toExternalAppMsgFn


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

        _ ->
            ( socket, Cmd.none )


{-| pushs a message
-}
push : Push msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
push pushRecord socket =
    let
        event =
            Event pushRecord.event pushRecord.channel.topic pushRecord.payload (Just socket.ref)

        updateSocket =
            addEvent event socket
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
                |> addEvent event
                |> addChannel updatedChannel
    in
        -- LongPoll ->
        --        (updateSocket, LongPoll.send socket.endPoint socket.longPollToken event)
        ( updateSocket, InternalWebSocket.send socket.endPoint event )


addEvent : Event -> Socket msg -> Socket msg
addEvent event socket =
    { socket | pushedEvents = Dict.insert socket.ref event socket.pushedEvents, ref = socket.ref + 1 }


addChannel : Channel msg -> Socket msg -> Socket msg
addChannel channel socket =
    { socket | channels = Channel.addChannel channel socket.channels }