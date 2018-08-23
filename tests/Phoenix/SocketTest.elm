module Phoenix.SocketTest exposing (TestMsg(..), basicEndpoint, endPointFuzzer, suite)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Phoenix.Channel as Channel
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Phoenix.Message as Message
import Phoenix.Push as Push
import Phoenix.Socket as Socket
import Test exposing (..)
import Time


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
                    Expect.equal (Socket.endPoint socket) endPoint
            ]
        , describe "initialise socket with channel"
            [ test "default init should return empty channel list" <|
                \_ ->
                    let
                        socket =
                            Socket.init basicEndpoint
                    in
                    Expect.equal (Dict.size (Socket.channels socket)) 0
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
                    Expect.equal (Dict.size (Socket.channels socket)) 1
            , test "subscribe should add an item to list of channels" <|
                \_ ->
                    let
                        channel =
                            Channel.init "chat:room233"

                        ( socket, cmd ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.subscribe channel
                    in
                    Expect.equal (Dict.size (Socket.channels socket)) 1
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
                    Expect.equal (Dict.size (Socket.channels socket)) 1
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
                    case Dict.get (Channel.topic channel) (Socket.channels socket) of
                        Just ch ->
                            Expect.equal (Channel.isOngoing ch) True

                        _ ->
                            Expect.fail "not possible!"
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
                            Channel.findChannel (Channel.topic channel) (Socket.channels updatedSocket)
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
                            Channel.findChannel (Channel.topic channel) (Socket.channels updatedSocket)
                    in
                    case errChannel of
                        Just ec ->
                            Expect.equal (Channel.isErrored ec) True

                        _ ->
                            Expect.fail "couldn't find the channel!"
            , test "ChannelError when receive event phx_error with topic of channel" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init
                                |> Channel.onError ChannelGotError

                        value =
                            Encode.object []

                        ( socket, _ ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel

                        msg =
                            Message.toInternalMsg (ChannelError channel value)

                        ( updatedSocket, _ ) =
                            Socket.update PhoenixMsg msg socket

                        errChannel =
                            Channel.findChannel (Channel.topic channel) (Socket.channels updatedSocket)
                    in
                    case errChannel of
                        Just ec ->
                            Expect.equal (Channel.isErrored ec) True

                        _ ->
                            Expect.fail "couldn't find the channel!"
            , test "ChannelClosed when server closes channel" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init
                                |> Channel.onClose ChannelGotClosed

                        value =
                            Encode.object []

                        ( socket, _ ) =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel

                        msg =
                            Message.toInternalMsg (ChannelClosed channel value)

                        ( updatedSocket, _ ) =
                            Socket.update PhoenixMsg msg socket

                        errChannel =
                            Channel.findChannel (Channel.topic channel) (Socket.channels updatedSocket)
                    in
                    case errChannel of
                        Just ec ->
                            Expect.equal (Channel.isClosed ec) True

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
                            Message.toInternalMsg (Heartbeat (Time.millisToPosix 19292922))

                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.join channel
                                |> Tuple.first
                                |> Socket.update PhoenixMsg msg
                                |> Tuple.first
                    in
                    Expect.equal (Socket.heartbeatTimestamp socket) (Time.millisToPosix 19292922)
            ]
        , describe "pushs event"
            [ test "push an event" <|
                \_ ->
                    let
                        channel =
                            "chat:room233"
                                |> Channel.init

                        payload =
                            Encode.object [ ( "name", Encode.string "foo" ) ]

                        push =
                            Push.initWithChannel "hello" channel
                                |> Push.withPayload payload
                                |> Push.onOk OnOkCmd

                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.push push
                                |> Tuple.first
                    in
                    case Dict.get 1 (Socket.pushedEvents socket) of
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
                            Push.initWithChannel "hello" channel
                                |> Push.withPayload payload
                                |> Push.onOk OnOkCmd

                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.push push
                                |> Tuple.first
                                |> Socket.push push
                                |> Tuple.first
                    in
                    case Dict.get 2 (Socket.pushedEvents socket) of
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
    | ChannelGotError Value
    | ChannelGotClosed Value
    | OnOkCmd Value


endPointFuzzer : Fuzzer String
endPointFuzzer =
    Fuzz.string
        |> Fuzz.map (\s -> "ws://" ++ s)
