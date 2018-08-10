module Phoenix.SocketTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel


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
        , describe "join a channel"
            [ test "add channel to socket channel list" <|
                \_ ->
                    Expect.equal 1 1
            ]
        ]


endPointFuzzer : Fuzzer String
endPointFuzzer =
    Fuzz.string
        |> Fuzz.map (\s -> "ws://" ++ s)
