module Phoenix.Channel
    exposing
        ( Channel
        , addChannel
        , findChannel
        , findChannelWithRef
        , init
        , isClosed
        , isErrored
        , isJoined
        , isOngoing
        , on
        , onClose
        , onError
        , onJoin
        , onJoinError
        , setErroredState
        , setJoinedState
        , setJoiningState
        , updateChannel
        )

{-|


# This module is keeping states related to channel

@docs Channel, init, setJoiningState, setJoinedState, setErroredState, isOngoing, isClosed, isJoined, isErrored, addChannel, updateChannel, findChannelWithRef, findChannel, on, onJoin, onJoinError, onError, onClose

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
type alias Channel msg =
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
init topic =
    { topic = topic
    , on = Dict.empty
    , state = Init
    , joinRef = Nothing
    , receive = Dict.empty
    , payload = Encode.object []
    }


{-| Sets the joining reference and state to Joining
-}
setJoiningState : Int -> Channel msg -> Channel msg
setJoiningState ref channel =
    { channel | state = Joining, joinRef = Just ref }


{-| Sets stats to joined state
-}
setJoinedState : Channel msg -> Channel msg
setJoinedState channel =
    { channel | state = Joined, joinRef = Nothing }


{-| Sets stats to Errored
-}
setErroredState : Channel msg -> Channel msg
setErroredState channel =
    { channel | state = Errored, joinRef = Nothing }


{-| Returns true if state is Joined Joining
-}
isOngoing : Channel msg -> Bool
isOngoing channel =
    if channel.state == Joining || channel.state == Joined then
        True

    else
        False


{-| Is this channel joined successfully ?
-}
isJoined : Channel msg -> Bool
isJoined channel =
    case channel.state of
        Joined ->
            True

        _ ->
            False


{-| Is this channel closed ?
-}
isClosed : Channel msg -> Bool
isClosed channel =
    case channel.state of
        Closed ->
            True

        _ ->
            False


{-| Is this channel faild to join
-}
isErrored : Channel msg -> Bool
isErrored channel =
    case channel.state of
        Errored ->
            True

        _ ->
            False


{-| Adds a channel to Dict of channels
-}
addChannel : Channel msg -> Dict String (Channel msg) -> Dict String (Channel msg)
addChannel channel channelDict =
    Dict.insert channel.topic channel channelDict


{-| Finds a channel with its topic and joinRef number
-}
findChannelWithRef : String -> Maybe Int -> Dict String (Channel msg) -> Maybe (Channel msg)
findChannelWithRef topic joinRef channels =
    case Dict.get topic channels of
        Just channel ->
            if channel.joinRef == joinRef then
                Just channel

            else
                Nothing

        _ ->
            Nothing


{-| Finds a channel in Dict of channels by its topic
-}
findChannel : String -> Dict String (Channel msg) -> Maybe (Channel msg)
findChannel topic channels =
    Dict.get topic channels


{-| Updates channel in the given Dict
-}
updateChannel : Channel msg -> Dict String (Channel msg) -> Dict String (Channel msg)
updateChannel channel channels =
    Dict.insert channel.topic channel channels


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
-}
onError : (Value -> msg) -> Channel msg -> Channel msg
onError valueToMsg channel =
    receive "error" valueToMsg channel


{-| Triggers this message when channel is closed
-}
onClose : (Value -> msg) -> Channel msg -> Channel msg
onClose valueToMsg channel =
    receive "close" valueToMsg channel


{-| Triggers this message when event is received
-}
on : String -> (Value -> msg) -> Channel msg -> Channel msg
on event cb channel =
    { channel | on = Dict.insert event cb channel.on }


receive : String -> (Value -> msg) -> Channel msg -> Channel msg
receive event valueToMsg channel =
    { channel | receive = Dict.insert event valueToMsg channel.receive }
