module Phoenix.Event exposing (Event, init, decode, encode)

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



-- (Maybe.map Encode.int) >> (Maybe.withDefault Encode.null)
