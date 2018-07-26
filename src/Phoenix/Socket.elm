module Phoenix.Socket exposing (init, Socket, listen, join)

{-|
# Basic Usage

@docs init, Socket, listen, join

-}

import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Event as Event exposing (Event)
import Dict exposing (Dict)
import WebSocket
import Json.Encode as Encode


type Transport
    = WebSocket
    | LongPoll


{-| Socket options
-}
type alias Socket msg =
    { endPoint : String
    , transport : Transport
    , channels : Dict String (Channel msg)
    , pushedEvents : Dict Int Event
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
    , ref = 1
    }


{-| What ever
-}
listen : Socket msg -> (Msg msg -> msg) -> Sub msg
listen socket fn =
    let
        mappedMsg =
            Message.mapAll fn

        subs =
            Sub.batch [ internalMsgs socket ]
    in
        Sub.map mappedMsg subs



--  (Sub.batch >> mapper)
--   [ internalMsgs socket ]
-- WebSocket.listen socket.endPoint Message.decodeEvent


internalMsgs : Socket msg -> Sub (Msg msg)
internalMsgs socket =
    Sub.map (mapInternalMsgs socket) (phoenixMessages socket)


mapInternalMsgs : Socket msg -> Maybe Event -> Msg msg
mapInternalMsgs socket maybeEvent =
    case maybeEvent of
        Just event ->
            case event.event of
                "phx_reply" ->
                    let
                        _ =
                            Debug.log "phx_reply" event

                        -- phx_reply: { event = "phx_reply", topic = "numbers:positive", payload = { status = "ok", response = { mynameis = "milad" } } }
                    in
                        Message.none

                _ ->
                    Message.none

        _ ->
            Message.none


phoenixMessages : Socket msg -> Sub (Maybe Event)
phoenixMessages socket =
    WebSocket.listen socket.endPoint Event.decode


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
        channel_ =
            Channel.setJoiningState socket.ref channel

        socket_ =
            addChannel channel socket
    in
        pushEvent "phx_join" channel_ socket_


pushEvent : String -> Channel msg -> Socket msg -> ( Socket msg, Cmd (Msg msg) )
pushEvent eventName channel socket =
    let
        event =
            Event.init eventName channel.topic channel.payload (Just socket.ref)

        socket_ =
            addEvent event socket
    in
        ( socket
        , send socket event.event event.topic event.payload
        )


send : Socket msg -> String -> String -> Encode.Value -> Cmd (Msg msg)
send { endPoint, ref } event channel payload =
    sendMessage endPoint (Event event channel payload (Just ref))


sendMessage : String -> Event -> Cmd (Msg msg)
sendMessage path message =
    WebSocket.send path (Event.encode message)


addEvent : Event -> Socket msg -> Socket msg
addEvent event socket =
    { socket | pushedEvents = Dict.insert socket.ref event socket.pushedEvents, ref = socket.ref + 1 }
