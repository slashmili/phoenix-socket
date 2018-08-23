module Phoenix.Channel exposing (Channel, init, setJoiningState, setClosedState, setJoinedState, setErroredState, isOngoing, isClosed, isJoined, isErrored, addChannel, updateChannel, updateChannelDict, findChannelWithRef, findChannel, on, onJoin, onJoinError, onError, onClose, topic, joinRef, receives, ons, payload)

{-|


# This module is keeping states related to channel

@docs Channel, init, setJoiningState, setClosedState, setJoinedState, setErroredState, isOngoing, isClosed, isJoined, isErrored, addChannel, updateChannel, updateChannelDict, findChannelWithRef, findChannel, on, onJoin, onJoinError, onError, onClose, topic, joinRef, receives, ons, payload

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode


type State
    = Init
    | Closed
    | Errored
    | Joined
    | Joining
    | Leaving


{-| Channel Model
-}
type Channel msg
    = Channel
        { topic : String
        , on : Dict String (Value -> msg)
        , state : State
        , joinRef : Maybe Int
        , receive : Dict String (Value -> msg)
        , payload : Encode.Value
        }


{-| Init channel model using channel topic
-}
init : String -> Channel msg
init channelTopic =
    Channel
        { topic = channelTopic
        , on = Dict.empty
        , state = Init
        , joinRef = Nothing
        , receive = Dict.empty
        , payload = Encode.object []
        }


{-| Sets the joining reference and state to Joining
-}
setJoiningState : Int -> Channel msg -> Channel msg
setJoiningState ref (Channel channel) =
    Channel { channel | state = Joining, joinRef = Just ref }


{-| Sets stats to joined state
-}
setJoinedState : Channel msg -> Channel msg
setJoinedState (Channel channel) =
    Channel { channel | state = Joined, joinRef = Nothing }


{-| Sets stats to Errored
-}
setErroredState : Channel msg -> Channel msg
setErroredState (Channel channel) =
    Channel { channel | state = Errored, joinRef = Nothing }


{-| Sets stats to Closed
-}
setClosedState : Channel msg -> Channel msg
setClosedState (Channel channel) =
    Channel { channel | state = Closed, joinRef = Nothing }


{-| Returns true if state is Joined Joining
-}
isOngoing : Channel msg -> Bool
isOngoing (Channel channel) =
    if channel.state == Joining || channel.state == Joined then
        True

    else
        False


{-| Is this channel joined successfully ?
-}
isJoined : Channel msg -> Bool
isJoined (Channel channel) =
    case channel.state of
        Joined ->
            True

        _ ->
            False


{-| Is this channel closed ?
-}
isClosed : Channel msg -> Bool
isClosed (Channel channel) =
    case channel.state of
        Closed ->
            True

        _ ->
            False


{-| Is this channel faild to join
-}
isErrored : Channel msg -> Bool
isErrored (Channel channel) =
    case channel.state of
        Errored ->
            True

        _ ->
            False


{-| Adds a channel to Dict of channels
-}
addChannel : Channel msg -> Dict String (Channel msg) -> Dict String (Channel msg)
addChannel (Channel channel) channelDict =
    Dict.insert channel.topic (Channel channel) channelDict


{-| Finds a channel with its topic and joinRef number
-}
findChannelWithRef : String -> Maybe Int -> Dict String (Channel msg) -> Maybe (Channel msg)
findChannelWithRef channelTopic channelJoinRef channels =
    case Dict.get channelTopic channels of
        Just (Channel channel) ->
            if channel.joinRef == channelJoinRef then
                Just (Channel channel)

            else
                Nothing

        _ ->
            Nothing


{-| Finds a channel in Dict of channels by its topic
-}
findChannel : String -> Dict String (Channel msg) -> Maybe (Channel msg)
findChannel channelTopic channels =
    Dict.get channelTopic channels


{-| Updates channel in the given Dict
-}
updateChannel : Channel msg -> Dict String (Channel msg) -> Dict String (Channel msg)
updateChannel (Channel channel) channels =
    Dict.insert channel.topic (Channel channel) channels


{-| Triggers this message to send when joined a channel
-}
onJoin : (Value -> msg) -> Channel msg -> Channel msg
onJoin valueToMsg channel =
    receive "ok" valueToMsg channel


{-| Triggers this message to send when failed to join a channel
-}
onJoinError : (Value -> msg) -> Channel msg -> Channel msg
onJoinError valueToMsg channel =
    receive "join_error" valueToMsg channel


{-| Triggers this message when failed to send join command on the connection

onError messages are invoked if the channel crashes on the server.

-}
onError : (Value -> msg) -> Channel msg -> Channel msg
onError valueToMsg channel =
    receive "error" valueToMsg channel


{-| Triggers this message when channel is closed

onClose messages are invoked if the channel explicitly closed on the server.

-}
onClose : (Value -> msg) -> Channel msg -> Channel msg
onClose valueToMsg channel =
    receive "close" valueToMsg channel


{-| Triggers this message when event is received
-}
on : String -> (Value -> msg) -> Channel msg -> Channel msg
on event cb (Channel channel) =
    Channel { channel | on = Dict.insert event cb channel.on }


receive : String -> (Value -> msg) -> Channel msg -> Channel msg
receive event valueToMsg (Channel channel) =
    Channel { channel | receive = Dict.insert event valueToMsg channel.receive }


{-| -}
updateChannelDict : Channel msg -> Dict String (Channel msg) -> Dict String (Channel msg)
updateChannelDict channel channels =
    updateChannel channel channels


{-| -}
topic : Channel msg -> String
topic (Channel channel) =
    channel.topic


{-| -}
joinRef : Channel msg -> Maybe Int
joinRef (Channel channel) =
    channel.joinRef


{-| -}
receives : Channel msg -> Dict String (Value -> msg)
receives (Channel channel) =
    channel.receive


{-| -}
ons : Channel msg -> Dict String (Value -> msg)
ons (Channel channel) =
    channel.on


{-| -}
payload : Channel msg -> Encode.Value
payload (Channel channel) =
    channel.payload
