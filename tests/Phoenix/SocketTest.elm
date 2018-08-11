module Phoenix.SocketTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Phoenix.Message as Message
import Phoenix.Push as Push
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode


basicEndpoint =
    "ws://localhost:4000/socket/websocket"


suite : Test
suite =
    describe "initialise socket"
        [ describe "Socket.init"
            [ fuzz endPointFuzzer "initialize Socket" <|
                \endPoint ->
                    let
                        socket =
                            Socket.init endPoint
                    in
                        Expect.equal socket.endPoint endPoint
            ]
        , describe "initialise socket with channel"
            [ test "default init should return empty channel list" <|
                \_ ->
                    let
                        socket =
                            Socket.init basicEndpoint
                    in
                        Expect.equal (Dict.size socket.channels) 0
            , test "join should add an item to list of channels" <|
                \_ ->
                    let
                        channel =
                            Channel.init "chat:room233"

                        ( socket, cmd ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel
                    in
                        Expect.equal (Dict.size socket.channels) 1
            , test "calling join again on a channel that is not joined yet should be ignored" <|
                \_ ->
                    let
                        channel =
                            Channel.init "chat:room233"

                        ( socket, cmd ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel
                                |> Tuple.first
                                |> Socket.join channel
                    in
                        Expect.equal (Dict.size socket.channels) 1
            , test "join should add an item to list of pushedEvents" <|
                \_ ->
                    let
                        channel =
                            Channel.init "chat:room233"

                        ( socket, cmd ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel
                    in
                        Expect.equal (Dict.size socket.pushedEvents) 1
            , test "Join event name should be phx_join" <|
                \_ ->
                    let
                        channel =
                            Channel.init "chat:room233"

                        ( socket, cmd ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel
                    in
                        case Dict.get 1 socket.pushedEvents of
                            Just event ->
                                Expect.equal event.event "phx_join"

                            Nothing ->
                                Expect.fail "didn't find event"
            ]
        , describe "listen to socket"
            [ test "return subscription" <|
                \_ ->
                    let
                        socket =
                            Socket.init basicEndpoint

                        sub =
                            Socket.listen PhoenixMsg socket
                    in
                        Expect.notEqual sub Sub.none
            ]
        , describe "updates socket model"
            [ test "none" <|
                \_ ->
                    let
                        socket =
                            basicEndpoint
                                |> Socket.init

                        msg =
                            Message.toInternalMsg NoOp

                        ( updatedSocket, cmd ) =
                            Socket.update PhoenixMsg msg socket
                    in
                        Expect.equal cmd Cmd.none
            , test "ChannelSuccessfullyJoined" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init
                                |> Channel.onJoin JoinedChannel

                        value =
                            Encode.object []

                        ( socket, _ ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel

                        msg =
                            Message.toInternalMsg (ChannelSuccessfullyJoined channel value)

                        ( updatedSocket, _ ) =
                            Socket.update PhoenixMsg msg socket

                        joinedChannel =
                            Channel.findChannel channel.topic updatedSocket.channels
                    in
                        case joinedChannel of
                            Just jc ->
                                Expect.equal (Channel.isJoined jc) True

                            _ ->
                                Expect.fail "couldn't find the channel!"
            , test "ChannelFailedToJoin" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init
                                |> Channel.onJoinError FailedToJoinedChannel

                        value =
                            Encode.object []

                        ( socket, _ ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel

                        msg =
                            Message.toInternalMsg (ChannelFailedToJoin channel value)

                        ( updatedSocket, _ ) =
                            Socket.update PhoenixMsg msg socket

                        errChannel =
                            Channel.findChannel channel.topic updatedSocket.channels
                    in
                        case errChannel of
                            Just ec ->
                                Expect.equal (Channel.isErrored ec) True

                            _ ->
                                Expect.fail "couldn't find the channel!"
            , test "Heartbeat" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init

                        value =
                            Encode.object []

                        msg =
                            Message.toInternalMsg (Heartbeat 19292922)

                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel
                                |> Tuple.first
                                |> Socket.update PhoenixMsg msg
                                |> Tuple.first
                                |> Socket.update PhoenixMsg msg
                                |> Tuple.first

                        errChannel =
                            Channel.findChannel channel.topic socket.channels

                        event =
                            Dict.get 2 socket.pushedEvents
                    in
                        case Dict.get 2 socket.pushedEvents of
                            Just event ->
                                Expect.equal event.event "heartbeat"

                            _ ->
                                Expect.fail "couldn't find second heartbeat"
            ]
        , describe "pushs event"
            [ test "push and event" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init

                        payload =
                            Encode.object [ ( "name", Encode.string "foo" ) ]

                        push =
                            Push.init "hello" channel
                                |> Push.withPayload payload

                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.push push
                                |> Tuple.first
                    in
                        case Dict.get 1 socket.pushedEvents of
                            Just event ->
                                Expect.equal event.payload payload

                            _ ->
                                Expect.fail "couldn't find pushed event"
            , test "push second event" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init

                        payload =
                            Encode.object [ ( "name", Encode.string "foo" ) ]

                        push =
                            Push.init "hello" channel
                                |> Push.withPayload payload

                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.push push
                                |> Tuple.first
                                |> Socket.push push
                                |> Tuple.first
                    in
                        case Dict.get 2 socket.pushedEvents of
                            Just event ->
                                Expect.equal event.payload payload

                            _ ->
                                Expect.fail "couldn't find second pushed event"
            ]
        ]


type TestMsg
    = PhoenixMsg (Message.Msg TestMsg)
    | JoinedChannel Value
    | FailedToJoinedChannel Value


endPointFuzzer : Fuzzer String
endPointFuzzer =
    Fuzz.string
        |> Fuzz.map (\s -> "ws://" ++ s)
