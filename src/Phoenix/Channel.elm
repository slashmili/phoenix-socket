module Phoenix.Channel exposing (init, on, Channel, isOngoing, setJoiningState)

{-|
# Basic Usage

@docs init, on, isOngoing, setJoiningState

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
    , payload : Encode.Value
    }


init : String -> Channel msg
init topic =
    { topic = topic
    , on = Dict.empty
    , state = Init
    , joinRef = Nothing
    , payload = Encode.object []
    }

on : String -> (Value -> msg) -> Channel msg -> Channel msg
on event cb channel=
    { channel | on = Dict.insert event cb channel.on }

{-|
Return true if connection has joined the channel or is joining
-}
isOngoing : Channel msg -> Bool
isOngoing channel =
    if channel.state == Joining || channel.state == Joined then
       True
   else
       False

setJoiningState: Int -> Channel msg -> Channel msg
setJoiningState ref channel =
    {channel | state = Joining, joinRef = Just ref}
