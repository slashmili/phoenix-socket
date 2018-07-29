module Phoenix.Socket exposing (init, withLongPoll, Socket, listen, join, update, push)

{-|
# Basic Usage

@docs init, withLongPoll, Socket, listen, join, update, push

-}

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.ChannelHelper as ChannelHelper
import Phoenix.Message as Message exposing (Msg(..))
import Phoenix.Event as Event exposing (Event)
import Phoenix.Push as Push exposing (Push)
import Phoenix.Internal.WebSocket as InternalWebSocket
import Dict exposing (Dict)
import Task exposing (Task)
import Json.Encode as Encode


type Transport
    = WebSocket
    | LongPoll


type Status
    = Diconnected
    | Connected


{-| Socket options
-}
type alias Socket msg =
    { endPoint : String
    , transport : Transport
    , channels : Dict String (Channel msg)
    , pushedEvents : Dict Int Event
    , status : Status
    , ref : Int
    }


{-| initialize a Socket

-}
init : String -> Socket msg
init endPoint =
    { endPoint = endPoint
    , transport = WebSocket
    , channels = Dict.empty
    , pushedEvents = Dict.empty
    , status = Diconnected
    , ref = 1
    }


{-| withLongPoll
-}
withLongPoll : Socket msg -> Socket msg
withLongPoll socket =
    { socket
        | transport = LongPoll
        , endPoint = "http://localhost:4000/socket/longpoll"
    }


{-| What ever
-}
listen : Socket msg -> (Msg msg -> msg) -> Sub msg
listen socket toExternalAppMsgFn =
    case socket.transport of
        WebSocket ->
            InternalWebSocket.listen socket.endPoint socket.channels toExternalAppMsgFn
        LongPoll ->
            Sub.map toExternalAppMsgFn Sub.none


addChannel : Channel msg -> Socket msg -> Socket msg
addChannel channel socket =
    { socket | channels = Dict.insert channel.topic channel socket.channels }


{-| join
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


doJoin : Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
doJoin channel socket =
    let
        eventName = "phx_join"
        updatedChannel =
            Channel.setJoiningState socket.ref channel
        event =
            Event.init eventName channel.topic channel.payload (Just socket.ref)

        updateSocket =
            socket
            |> addEvent event
            |> addChannel updatedChannel

    in
       case socket.transport of
           WebSocket ->
               (updateSocket, InternalWebSocket.send socket.endPoint event)
           LongPoll ->
               (updateSocket, Cmd.none)


addEvent : Event -> Socket msg -> Socket msg
addEvent event socket =
    { socket | pushedEvents = Dict.insert socket.ref event socket.pushedEvents, ref = socket.ref + 1 }


{-| update
-}
update : (Msg msg -> msg) -> Msg msg -> Socket msg -> ( Socket msg, Cmd msg )
update toExternalAppMsgFn msg socket =
    case msg of
        ChannelSuccessfullyJoined channel response ->
            let
                updatedChannel =
                    Channel.setJoinedState (channel)

                updateSocket =
                    { socket | channels = Channel.updateChannelDict updatedChannel socket.channels }
            in
                ( updateSocket, Cmd.none )

        ChannelFailedToJoin channel response ->
            let
                updatedChannel =
                    Channel.setJoinedState (channel)

                updateSocket =
                    { socket | channels = Channel.updateChannelDict updatedChannel socket.channels }
            in
                ( updateSocket, Cmd.none )

        _ ->
            let
                _ =
                    Debug.log "Socket update" msg
            in
                ( socket, Cmd.none )


{-| push
-}
push : Push msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
push pushRecord socket =
    let
        event = Event pushRecord.event pushRecord.channel.topic pushRecord.payload (Just socket.ref)
        updateSocket = addEvent event socket
    in
       case socket.transport of
           WebSocket ->
               (updateSocket, InternalWebSocket.send socket.endPoint event)
           LongPoll ->
               (updateSocket, Cmd.none)
