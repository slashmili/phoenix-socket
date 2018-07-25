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
            , test "withChannel should add an item to list of channels" <|
                \_ ->
                    let
                        socket =
                            basicEndpoint
                                |> Socket.init
                                |> Socket.withChannel (Channel.init "foo:bar")
                    in
                        Expect.equal (Dict.size socket.channels) 1
            ]
        ]


endPointFuzzer : Fuzzer String
endPointFuzzer =
    Fuzz.string
        |> Fuzz.map (\s -> "ws://" ++ s)
