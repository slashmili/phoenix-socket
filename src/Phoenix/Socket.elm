module Phoenix.Socket exposing (Socket, init, join)

{-|
# This module provides an interface for connecting to Phoenix Socket

@docs Socket, init, join
-}

import Dict exposing (Dict)
import Phoenix.Serializer exposing (Serializer(..))
import Phoenix.Internal.WebSocket as InternalWebSocket
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)


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
