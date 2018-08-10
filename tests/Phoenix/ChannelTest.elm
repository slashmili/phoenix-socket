module Phoenix.ChannelTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict
import Phoenix.Channel as Channel


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
                        Expect.equal channel.topic topic
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
                        Expect.equal channel.joinRef (Just joinRef)
            , fuzz2 Fuzz.string Fuzz.int "set joining state" <|
                \topic joinRef ->
                    let
                        channel =
                            Channel.init topic

                        channelJoining =
                            Channel.setJoiningState joinRef channel
                    in
                        Expect.notEqual channel.state channelJoining.state
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
                        Expect.equal (Channel.findChannel channel.topic Dict.empty) Nothing
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
                        Expect.equal (Channel.findChannel channel.topic channelDict) (Just channel)
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
                        Expect.equal (Channel.findChannelWithRef channel.topic (Just joinRef) channelDict) (Just channel)
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
                        Expect.equal (Channel.findChannelWithRef channel.topic (Just 2) channelDict) Nothing
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
        ]
