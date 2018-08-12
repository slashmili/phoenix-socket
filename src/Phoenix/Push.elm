module Phoenix.Push exposing (Push, init, initWithChannel, onError, onErrorCommand, onOk, onOkCommand, withPayload)

{-|


# This module provides an interface for pushed messages

@docs Push, init, initWithChannel, onError, onErrorCommand, onOk, onOkCommand, withPayload

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Phoenix.Channel exposing (Channel)
import Phoenix.Message as Message exposing (Msg)


{-| Push model
-}
type alias Push msg =
    { event : String
    , topic : String
    , on : Dict String (Decode.Value -> msg)
    , payload : Encode.Value
    }


{-| initializes Push with event name and the channel
-}
init : String -> String -> Push msg
init event topic =
    { event = event, topic = topic, payload = Encode.object [], on = Dict.empty }


{-| initializes Push with event name and the channel
-}
initWithChannel : String -> Channel msg -> Push msg
initWithChannel event channel =
    { event = event, topic = channel.topic, payload = Encode.object [], on = Dict.empty }


{-| Adds payload to Push
-}
withPayload : Encode.Value -> Push msg -> Push msg
withPayload payload push =
    { push | payload = payload }


{-| When Channel replies with ok this message gets triggred
-}
onOk : (Decode.Value -> msg) -> Push msg -> Push msg
onOk valueToMsg push =
    let
        on =
            Dict.insert "ok" valueToMsg push.on
    in
    { push | on = on }


{-| When Channel replies with errro this message gets triggred
-}
onError : (Decode.Value -> msg) -> Push msg -> Push msg
onError valueToMsg push =
    let
        on =
            Dict.insert "error" valueToMsg push.on
    in
    { push | on = on }


onReceiveMsg : String -> Decode.Value -> Push msg -> Msg msg
onReceiveMsg name response push =
    case Dict.get name push.on of
        Just cmd ->
            Message.toExternalMsg (cmd response)

        _ ->
            Message.none


{-| Convert an Ok response to Message
-}
onOkCommand : Decode.Value -> Push msg -> Msg msg
onOkCommand response push =
    onReceiveMsg "ok" response push


{-| Convert a Error response to Message
-}
onErrorCommand : Decode.Value -> Push msg -> Msg msg
onErrorCommand response push =
    onReceiveMsg "error" response push
