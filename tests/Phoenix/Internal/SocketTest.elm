module Phoenix.Internal.SocketTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Encode as Encode
import Phoenix.Channel as Channel
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Phoenix.Internal.Socket as InternalSocket
import Phoenix.Message as Message
import Phoenix.Push as Push
import Test exposing (..)


suite : Test
suite =
    describe "internal socket"
        [ describe "Handle internal socket messages"
            [ test "parse a direct event from server" <|
                \_ ->
                    let
                        channel =
                            Channel.init "numbers:positive"
                                |> Channel.on "update" UpdateEvent

                        eventPayload =
                            Encode.object [ ( "number", Encode.int 802 ) ]

                        event =
                            Event.init "update" "numbers:positive" eventPayload Nothing

                        channels =
                            Channel.addChannel channel Dict.empty

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents Dict.empty channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (UpdateEvent eventPayload))
            , test "parse successful join channel reply" <|
                \_ ->
                    let
                        joinRef =
                            10

                        channel =
                            Channel.init "numbers:positive"
                                |> Channel.onJoin JoinedChannel
                                |> Channel.setJoiningState joinRef

                        eventPayload =
                            Encode.object [ ( "initial", Encode.int 0 ) ]

                        status =
                            Encode.object [ ( "status", Encode.string "ok" ), ( "response", eventPayload ) ]

                        event =
                            Event.init "phx_reply" "numbers:positive" status (Just joinRef)

                        channels =
                            Channel.addChannel channel Dict.empty

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents Dict.empty channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (JoinedChannel eventPayload))
            , test "parse unsuccessful join channel reply" <|
                \_ ->
                    let
                        joinRef =
                            10

                        channel =
                            Channel.init "numbers:positive"
                                |> Channel.onJoinError JoinedChannel
                                |> Channel.setJoiningState joinRef

                        eventPayload =
                            Encode.object [ ( "initial", Encode.int 20 ) ]

                        status =
                            Encode.object [ ( "status", Encode.string "error" ), ( "response", eventPayload ) ]

                        event =
                            Event.init "phx_reply" "numbers:positive" status (Just joinRef)

                        channels =
                            Channel.addChannel channel Dict.empty

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents Dict.empty channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (JoinedChannel eventPayload))
            , test "parse error event" <|
                \_ ->
                    let
                        channel =
                            Channel.init "numbers:positive"
                                |> Channel.onError OnChannelError

                        eventPayload =
                            Encode.object [ ( "error", Encode.int 402 ) ]

                        event =
                            Event.init "phx_error" "numbers:positive" eventPayload (Just 192)

                        channels =
                            Channel.addChannel channel Dict.empty

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents Dict.empty channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (OnChannelError eventPayload))
            , test "parse closed event" <|
                \_ ->
                    let
                        channel =
                            Channel.init "numbers:positive"
                                |> Channel.onClose OnChannelClosed

                        eventPayload =
                            Encode.object [ ( "error", Encode.string "closed" ) ]

                        event =
                            Event.init "phx_close" "numbers:positive" eventPayload (Just 2)

                        channels =
                            Channel.addChannel channel Dict.empty

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents Dict.empty channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (OnChannelClosed eventPayload))
            , test "parse an ok reply to a push event" <|
                \_ ->
                    let
                        joinRef =
                            16

                        channel =
                            Channel.init "numbers:positive"

                        push =
                            Push.initWithChannel "get_current_value" channel
                                |> Push.onOk OnPushedOk

                        eventPayload =
                            Encode.object [ ( "current_value", Encode.int 28 ) ]

                        status =
                            Encode.object [ ( "status", Encode.string "ok" ), ( "response", eventPayload ) ]

                        event =
                            Event.init "phx_reply" "numbers:positive" status (Just joinRef)

                        channels =
                            Channel.addChannel channel Dict.empty

                        pushedEvents =
                            Dict.fromList [ ( joinRef, push ) ]

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents pushedEvents channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (OnPushedOk eventPayload))
            , test "parse an error reply to a push event" <|
                \_ ->
                    let
                        joinRef =
                            16

                        channel =
                            Channel.init "numbers:positive"

                        push =
                            Push.initWithChannel "get_current_value" channel
                                |> Push.onError OnPushedError

                        eventPayload =
                            Encode.object [ ( "current_value", Encode.int 28 ) ]

                        status =
                            Encode.object [ ( "status", Encode.string "error" ), ( "response", eventPayload ) ]

                        event =
                            Event.init "phx_reply" "numbers:positive" status (Just joinRef)

                        channels =
                            Channel.addChannel channel Dict.empty

                        pushedEvents =
                            Dict.fromList [ ( joinRef, push ) ]

                        msg =
                            event
                                |> InternalSocket.mapExternalEvents pushedEvents channels
                                |> Message.extractExternalMsg
                    in
                    Expect.equal msg (Just (OnPushedError eventPayload))
            ]
        ]


type TestMsg
    = UpdateEvent Encode.Value
    | JoinedChannel Encode.Value
    | OnChannelError Encode.Value
    | OnChannelClosed Encode.Value
    | OnPushedOk Encode.Value
    | OnPushedError Encode.Value
