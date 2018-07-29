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
import Phoenix.Internal.LongPoll as LongPoll
import Dict exposing (Dict)
import Task exposing (Task)
import Json.Encode as Encode
import Time exposing (Time, second)


type Transport
    = WebSocket
    | LongPoll


type State
    = Connecting
    | Open
    | Closing
    | Closed


{-| Socket options
-}
type alias Socket msg =
    { endPoint : String
    , transport : Transport
    , channels : Dict String (Channel msg)
    , pushedEvents : Dict Int Event
    , readyState : State
    , ref : Int
    , longPollToken : Maybe String
    }


{-| initialize a Socket

-}
init : String -> Socket msg
init endPoint =
    { endPoint = endPoint
    , transport = WebSocket
    , channels = Dict.empty
    , pushedEvents = Dict.empty
    , readyState = Closed
    , ref = 1
    , longPollToken = Nothing
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
            let
                freq =
                    case socket.readyState of
                        Closed ->
                            second
                        Open ->
                            (5 * second)
                        _ ->
                            (10 * second)
            in
            Sub.map toExternalAppMsgFn (Time.every freq LongPollTick)


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
               (updateSocket, LongPoll.send socket.endPoint socket.longPollToken event)


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

        LongPollTick _ ->
            case socket.readyState of
                Open ->
                    ({socket| readyState= Connecting}, Cmd.map toExternalAppMsgFn (LongPoll.poll socket.endPoint socket.longPollToken))
                Connecting ->
                    (socket, Cmd.none)
                Closing ->
                    (socket, Cmd.none)
                Closed ->
                    ({socket| readyState= Connecting}, Cmd.map toExternalAppMsgFn (LongPoll.poll socket.endPoint socket.longPollToken))

        LongPollPolled (Ok longPollEvent) ->
            case longPollEvent.status of
                200 ->
                    -- TODO: got content!
                    ({socket | readyState = Closed}, Cmd.none)

                401 ->
                    -- TODO: send onJoinError Msg
                    (socket, Cmd.none)
                410 ->
                    -- connecten is opened
                    ({socket | readyState = Open, longPollToken = longPollEvent.token}, Cmd.none)
                204 ->
                    -- no content
                    ({socket | readyState = Open}, Cmd.none)
                _ ->
                    ({socket | readyState = Closed}, Cmd.none)
        LongPollPolled (Err _) ->
            ({socket | readyState = Closed}, Cmd.none)

        LongPollSent (Err _) ->
            (socket, Cmd.none)


        _ ->
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
