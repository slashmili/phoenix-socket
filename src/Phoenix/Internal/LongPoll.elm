module Phoenix.Internal.LongPoll exposing (LongPollResponse, externalMsgs, poll, send)

import Dict exposing (Dict)
import Http
import Json.Encode as Encode
import Phoenix.Channel as Channel exposing (Channel)
import Phoenix.Event as Event exposing (Event)
import Phoenix.Internal.Channel as ChannelHelper
import Phoenix.Internal.LongPollEvent exposing (longPolldecoder)
import Phoenix.Internal.Message as InternalMessage exposing (InternalMessage(..))
import Phoenix.Internal.Socket as SocketHelper
import Phoenix.Message as Message exposing (Msg)
import Phoenix.Push exposing (Push)
import Task


type alias LongPollResponse =
    { status : Int
    , token : String
    }


send : String -> Maybe String -> Event -> Cmd (Msg msg)
send endPoint maybeToken event =
    let
        qparam =
            case maybeToken of
                Just token ->
                    "?token=" ++ token

                _ ->
                    ""
    in
    longPolldecoder
        |> Http.post (endPoint ++ qparam) (Http.jsonBody (Event.encoder event))
        |> Http.send LongPollSent
        |> Cmd.map Message.toInternalMsg


poll : String -> List ( String, String ) -> Maybe String -> Cmd (Msg msg)
poll endPoint payload maybeToken =
    let
        qparams =
            case maybeToken of
                Nothing ->
                    payload
                        |> List.map (\( key, value ) -> key ++ "=" ++ value)
                        |> String.join "&"

                Just connectionToken ->
                    "token=" ++ connectionToken

        fullQueryParams =
            if String.length qparams == 0 then
                ""

            else
                "?" ++ qparams
    in
    Http.get (endPoint ++ fullQueryParams) longPolldecoder
        |> Http.send LongPollPolled
        |> Cmd.map Message.toInternalMsg


externalMsgs : Dict Int (Push msg) -> Dict.Dict String (Channel msg) -> (Msg msg -> msg) -> List Event -> Cmd msg
externalMsgs pushedEvents channels toExternalAppMsgFn events =
    let
        mappedMsg =
            Message.mapAll toExternalAppMsgFn

        msgs =
            events
                |> List.reverse
                |> List.map (\e -> SocketHelper.mapExternalEvents pushedEvents channels e)
    in
    msgs
        |> List.map (\m -> m |> Task.succeed |> Task.perform identity)
        |> Cmd.batch
        |> Cmd.map mappedMsg
