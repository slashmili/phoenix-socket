module Phoenix.ChannelTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Value)
import Phoenix.Channel as Channel
import Test exposing (..)


suite : Test
suite =
    describe "initialise channel"
        [ describe "Socket.init"
            [ fuzz Fuzz.string "initialize channel" <|
                \topic ->
                    let
                        channel =
                            Channel.init topic
                    in
                    Expect.equal (Channel.topic channel) topic
            ]
        , describe
            "Channel state"
            [ fuzz2 Fuzz.string Fuzz.int "set joining ref" <|
                \topic joinRef ->
                    let
                        channel =
                            topic
                                |> Channel.init
                                |> Channel.setJoiningState joinRef
                    in
                    Expect.equal (Channel.joinRef channel) (Just joinRef)

            --            , fuzz2 Fuzz.string Fuzz.int "set joining state" <|
            --                \topic joinRef ->
            --                    let
            --                        channel =
            --                            Channel.init topic
            --
            --                        channelJoining =
            --                            Channel.setJoiningState joinRef channel
            --                    in
            --                    Expect.notEqual channel.state channelJoining.state
            --            , fuzz2 Fuzz.string Fuzz.int "set joined state" <|
            --                \topic joinRef ->
            --                    let
            --                        channel =
            --                            topic
            --                                |> Channel.init
            --                                |> Channel.setJoiningState joinRef
            --
            --                        channelJoined =
            --                            Channel.setJoinedState channel
            --                    in
            --                    Expect.notEqual channel.state channelJoined.state
            , fuzz2 Fuzz.string Fuzz.int "new Channel state should not be on going" <|
                \topic joinRef ->
                    let
                        channel =
                            topic
                                |> Channel.init
                    in
                    Expect.equal (Channel.isOngoing channel) False
            , fuzz2 Fuzz.string Fuzz.int "Channel should be on going after setJoiningState is being called" <|
                \topic joinRef ->
                    let
                        channel =
                            topic
                                |> Channel.init
                                |> Channel.setJoiningState joinRef
                    in
                    Expect.equal (Channel.isOngoing channel) True
            ]
        , describe "Find a channel in Dict"
            [ fuzz Fuzz.string "find a channel in an empty Dict" <|
                \topic ->
                    let
                        channel =
                            Channel.init topic
                    in
                    Expect.equal (Channel.findChannel (Channel.topic channel) Dict.empty) Nothing
            , test "find a channel that is not in channel Dict" <|
                \_ ->
                    let
                        channel =
                            Channel.init "mytopic"

                        channelDict =
                            Channel.addChannel channel Dict.empty
                    in
                    Expect.equal (Channel.findChannel "othertopic" channelDict) Nothing
            , fuzz Fuzz.string "find a channel in a Dict by its topic" <|
                \topic ->
                    let
                        channel =
                            Channel.init topic

                        channelDict =
                            Channel.addChannel channel Dict.empty
                    in
                    Expect.equal (Channel.findChannel (Channel.topic channel) channelDict) (Just channel)
            , fuzz2 Fuzz.string Fuzz.int "find a channel in a Dict by its topic and joinRef" <|
                \topic joinRef ->
                    let
                        channel =
                            topic
                                |> Channel.init
                                |> Channel.setJoiningState joinRef

                        channelDict =
                            Channel.addChannel channel Dict.empty
                    in
                    Expect.equal (Channel.findChannelWithRef (Channel.topic channel) (Just joinRef) channelDict) (Just channel)
            , fuzz Fuzz.string "find a channel in a Dict by its topic and wrong joinRef" <|
                \topic ->
                    let
                        joinRef =
                            1

                        channel =
                            topic
                                |> Channel.init
                                |> Channel.setJoiningState joinRef

                        channelDict =
                            Channel.addChannel channel Dict.empty
                    in
                    Expect.equal (Channel.findChannelWithRef (Channel.topic channel) (Just 2) channelDict) Nothing
            , test "find a channel in a Dict by wrong topic and wrong joinRef" <|
                \_ ->
                    let
                        joinRef =
                            1

                        topic =
                            "chat:myroom"

                        channel =
                            topic
                                |> Channel.init
                                |> Channel.setJoiningState joinRef

                        channelDict =
                            Channel.addChannel channel Dict.empty
                    in
                    Expect.equal (Channel.findChannelWithRef "foo" (Just 2) channelDict) Nothing
            ]
        , describe "update a channel in channel dic"
            [ test "update a channel" <|
                \_ ->
                    let
                        channel =
                            Channel.init "chat:rootm1"

                        channels =
                            Channel.addChannel channel Dict.empty
                    in
                    Expect.equal (Channel.updateChannel channel channels) channels
            ]

        --        , describe "triggers messages"
        --            [ test "new channel has 0 receive trigger messages" <|
        --                \_ ->
        --                    Expect.equal (Dict.size (Channel.init "chan").receive) 0
        --            , test "onJoin" <|
        --                \_ ->
        --                    let
        --                        channel =
        --                            "chat:rootm1"
        --                                |> Channel.init
        --                                |> Channel.onJoin AppMessage
        --                    in
        --                    Expect.equal (Dict.get "ok" channel.receive) (Just AppMessage)
        --            , test "onJoinError" <|
        --                \_ ->
        --                    let
        --                        channel =
        --                            "chat:rootm1"
        --                                |> Channel.init
        --                                |> Channel.onJoinError AppMessage
        --                    in
        --                    Expect.equal (Dict.get "join_error" channel.receive) (Just AppMessage)
        --            , test "onError" <|
        --                \_ ->
        --                    let
        --                        channel =
        --                            "chat:rootm1"
        --                                |> Channel.init
        --                                |> Channel.onError AppMessage
        --                    in
        --                    Expect.equal (Dict.get "error" channel.receive) (Just AppMessage)
        --            , test "onClose" <|
        --                \_ ->
        --                    let
        --                        channel =
        --                            "chat:rootm1"
        --                                |> Channel.init
        --                                |> Channel.onClose AppMessage
        --                    in
        --                    Expect.equal (Dict.get "close" channel.receive) (Just AppMessage)
        --            , test "on" <|
        --                \_ ->
        --                    let
        --                        channel =
        --                            "chat:rootm1"
        --                                |> Channel.init
        --                                |> Channel.on "foo_event" AppMessage
        --                    in
        --                    Expect.equal (Dict.get "foo_event" channel.on) (Just AppMessage)
        --            ]
        ]


type TestMsg
    = AppMessage Value
