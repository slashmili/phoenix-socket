module Phoenix.Message exposing (Msg, extractExternalMsg, extractInternalMsg, mapAll, none, toExternalMsg, toInternalMsg)

{-|


# This module provides Msg that the package handles

@docs Msg, mapAll, none, toInternalMsg, toExternalMsg, extractInternalMsg, extractExternalMsg

-}

import Http
import Json.Decode as Decode
import Phoenix.Channel exposing (Channel)
import Phoenix.Event exposing (Event)
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Time exposing (Time)


{-| This Msg should be used in user's main app

    import Phoenix.Message as PhxMsg

    type MyAppMsg =
        ..
        | PhoenixMsg (PhxMsg.Msg MyAppMsg)

-}
type Msg msg
    = NoOp
    | ExternalMsg msg
    | InternalMsg (InternalMessage msg)


{-| -}
mapAll : (Msg msg -> msg) -> Msg msg -> msg
mapAll fn internalMsg =
    case internalMsg of
        ExternalMsg msg ->
            msg

        _ ->
            fn internalMsg


{-| -}
none : Msg msg
none =
    NoOp


{-| -}
toExternalMsg : msg -> Msg msg
toExternalMsg externalMsg =
    ExternalMsg externalMsg


{-| -}
toInternalMsg : InternalMessage msg -> Msg msg
toInternalMsg internalMsg =
    InternalMsg internalMsg


{-| -}
extractInternalMsg : Msg msg -> InternalMessage msg
extractInternalMsg publicMsg =
    case publicMsg of
        InternalMsg msg ->
            msg

        ExternalMsg msg ->
            InternalMessage.none

        NoOp ->
            InternalMessage.none


{-| -}
extractExternalMsg : Msg msg -> Maybe msg
extractExternalMsg publicMsg =
    case publicMsg of
        ExternalMsg msg ->
            Just msg

        _ ->
            Nothing
