module Phoenix.Channel
    exposing
        ( Channel
        , init
        , setJoiningState
        , isOngoing
        , addChannel
        , findChannelWithRef
        , findChannel
        )

{-|
# This module is keeping states related to channel

@docs Channel, init, setJoiningState, isOngoing, addChannel, findChannelWithRef, findChannel
-}

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Dict exposing (Dict)


type State
    = Init
    | Closed
    | Errored
    | Joined
    | Joining
    | Leaving


{-|
Channel Model
-}
type alias Channel msg =
    { topic : String
    , on : Dict String (Value -> msg)
    , state : State
    , joinRef : Maybe Int
    , receive : Dict String (Value -> msg)
    , payload : Encode.Value
    }


{-|
Init channel model using channel topic
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


{-|
Sets the joining reference and state to Joining
-}
setJoiningState : Int -> Channel msg -> Channel msg
setJoiningState ref channel =
    { channel | state = Joining, joinRef = Just ref }


{-| Returns true if state is Joined Joining
-}
isOngoing : Channel msg -> Bool
isOngoing channel =
    if channel.state == Joining || channel.state == Joined then
        True
    else
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
