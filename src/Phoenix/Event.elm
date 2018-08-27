module Phoenix.Event exposing (Event, decode, decodeReply, decoder, encode, encoder, init)

import Json.Decode as Decode exposing (Decoder, field, maybe)
import Json.Encode as Encode


type alias Event =
    { event : String
    , topic : String
    , payload : Decode.Value
    , ref : Maybe Int
    }


init : String -> String -> Decode.Value -> Maybe Int -> Event
init event topic payload ref =
    { event = event, topic = topic, payload = payload, ref = ref }


decode : String -> Maybe Event
decode =
    Decode.decodeString decoder >> Result.toMaybe


decoder : Decoder Event
decoder =
    Decode.map4 Event
        (field "event" Decode.string)
        (field "topic" Decode.string)
        (field "payload" Decode.value)
        (field "ref" (maybe Decode.int))


encode : Event -> String
encode =
    encoder >> Encode.encode 0


encoder : Event -> Encode.Value
encoder { event, topic, payload, ref } =
    Encode.object
        [ ( "event", Encode.string event )
        , ( "topic", Encode.string topic )
        , ( "payload", payload )
        , ( "ref", maybeInt ref )
        ]


maybeInt int_ =
    int_
        |> Maybe.map Encode.int
        |> Maybe.withDefault Encode.null


replyDecoder : Decode.Decoder (Result Decode.Value Decode.Value)
replyDecoder =
    Decode.map2 statusToResult
        (field "status" Decode.string)
        (field "response" Decode.value)


decodeReply : Decode.Value -> Result Decode.Value Decode.Value
decodeReply payload =
    case Decode.decodeValue replyDecoder payload of
        Ok (Ok response) ->
            Ok response

        Ok (Err response) ->
            Err response

        Err errMsg ->
            Err (Encode.object [ ( "reason", Encode.string "failed to parse response" ) ])


statusToResult : String -> Decode.Value -> Result Decode.Value Decode.Value
statusToResult status response =
    if status == "ok" then
        Ok response

    else
        Err response
