module Phoenix.Channel exposing (init, on, Channel)

{-|
# Basic Usage

@docs init, on

-}

import Json.Decode as Decode exposing (Value)
import Dict exposing (Dict)


type alias Channel msg =
    { topic : String
    , on : Dict String (Value -> msg)
    }


init : String -> Channel msg
init topic =
    { topic = topic
    , on = Dict.empty
    }

on : String -> (Value -> msg) -> Channel msg -> Channel msg
on event cb channel=
    { channel | on = Dict.insert event cb channel.on }
