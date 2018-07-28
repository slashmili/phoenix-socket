module Phoenix.Channel exposing (init, on, onClose, onError, onJoin, Channel, isOngoing, setJoiningState, setJoinedState, findChannelWithRef, findChannel, updateChannelDict)

{-|
# Basic Usage

@docs init, on, isOngoing, setJoiningState, findChannel

-}

import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Dict exposing (Dict)


type State
    = Init
    | Joined
    | Joining


type alias Channel msg =
    { topic : String
    , on : Dict String (Value -> msg)
    , state : State
    , joinRef : Maybe Int
    , receive : Dict String (Value -> msg)
    , payload : Encode.Value
    }


init : String -> Channel msg
init topic =
    { topic = topic
    , on = Dict.empty
    , state = Init
    , joinRef = Nothing
    , receive = Dict.empty
    , payload = Encode.object []
    }


on : String -> (Value -> msg) -> Channel msg -> Channel msg
on event cb channel =
    { channel | on = Dict.insert event cb channel.on }


receive : String -> (Value -> msg) -> Channel msg -> Channel msg
receive event valueToMsg channel =
    { channel | receive = Dict.insert event valueToMsg channel.receive }

onJoin : (Value -> msg) -> Channel msg -> Channel msg
onJoin valueToMsg channel =
    receive "ok" valueToMsg channel

onError : (Value -> msg) -> Channel msg -> Channel msg
onError valueToMsg channel =
    receive "error" valueToMsg channel

onClose : (Value -> msg) -> Channel msg -> Channel msg
onClose valueToMsg channel =
    receive "close" valueToMsg channel


{-|
Return true if connection has joined the channel or is joining
-}
isOngoing : Channel msg -> Bool
isOngoing channel =
    if channel.state == Joining || channel.state == Joined then
        True
    else
        False


setJoiningState : Int -> Channel msg -> Channel msg
setJoiningState ref channel =
    { channel | state = Joining, joinRef = Just ref }


setJoinedState : Channel msg -> Channel msg
setJoinedState channel =
    { channel | state = Joined, joinRef = Nothing }


updateChannelDict : Channel msg -> Dict String (Channel msg) -> Dict String (Channel msg)
updateChannelDict channel channels =
    Dict.insert channel.topic channel channels


{-| Something
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

{-| Something
-}
findChannel : String -> Dict String (Channel msg) -> Maybe (Channel msg)
findChannel topic channels =
    Dict.get topic channels
