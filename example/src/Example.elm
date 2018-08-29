module Example exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Phoenix
import Phoenix.Channel as Channel
import Phoenix.Message as PhxMsg
import Phoenix.Push as Push
import Phoenix.Socket as Socket


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { input : String
    , messages : List String
    , phxSocket : Socket.Socket Msg
    }


endPoint =
    "ws://localhost:4001/socket/websocket"


initSocket =
    endPoint
        |> Socket.init
        |> Socket.withLongPoll
        |> Socket.withPayload [ ( "access_token", "super_secret_t0ken!!!" ) ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [] initSocket
    , Cmd.none
    )



-- UPDATE


type Msg
    = Input String
    | JoinChannel
    | NewMessage String
    | NewJsonMessage Value
    | JoinedChannel Value
    | FailedToJoinedChannel Value
    | FailedToConnectChannel Value
    | ClosedChannel Value
    | SendMessage
    | LeaveChannel
    | GetType
    | PhoenixMsg (PhxMsg.Msg Msg)


channel =
    Channel.init "numbers:positive"
        |> Channel.onJoin JoinedChannel
        |> Channel.onJoinError FailedToJoinedChannel
        |> Channel.onError FailedToConnectChannel
        |> Channel.onClose ClosedChannel
        |> Channel.on "update" NewJsonMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    --update msg { input, messages, phxSocket } =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        JoinChannel ->
            let
                ( updatedSocketModel, newCommand ) =
                    Phoenix.join PhoenixMsg channel model.phxSocket
            in
            ( { model | input = "", phxSocket = updatedSocketModel }, newCommand )

        GetType ->
            let
                push =
                    channel
                        |> Push.initWithChannel "type"
                        |> Push.withPayload payload

                payload =
                    Encode.object []

                ( updatedSocketModel, newCommand ) =
                    Phoenix.push PhoenixMsg push model.phxSocket
            in
            ( { model | phxSocket = updatedSocketModel }, newCommand )

        LeaveChannel ->
            let
                payload =
                    Encode.object []

                push =
                    channel
                        |> Push.initWithChannel "phx_leave"
                        |> Push.withPayload payload

                ( updatedSocketModel, newCommand ) =
                    Phoenix.push PhoenixMsg push model.phxSocket
            in
            ( { model | phxSocket = updatedSocketModel }, newCommand )

        SendMessage ->
            let
                payload =
                    Encode.object [ ( "name", Encode.string "boo" ) ]

                push =
                    "numbers:positive"
                        |> Push.init "new:msg"
                        |> Push.withPayload payload

                ( updatedSocketModel, newCommand ) =
                    Phoenix.push PhoenixMsg push model.phxSocket
            in
            ( { model | phxSocket = updatedSocketModel }, newCommand )

        NewMessage str ->
            ( { model | messages = str :: model.messages }, Cmd.none )

        NewJsonMessage json ->
            ( { model | messages = toString json :: model.messages }, Cmd.none )

        JoinedChannel json ->
            ( { model | messages = "Joined channel with data: " :: toString json :: model.messages }, Cmd.none )

        ClosedChannel json ->
            ( { model | messages = "ClosedChannel : " :: toString json :: model.messages }, Cmd.none )

        FailedToJoinedChannel json ->
            ( { model | messages = "Failed to joined channel with data: " :: toString json :: model.messages }, Cmd.none )

        FailedToConnectChannel json ->
            ( { model | messages = "Failed to connect: " :: toString json :: model.messages }, Cmd.none )

        PhoenixMsg innerMsg ->
            let
                ( updatedSocketModel, newCommand ) =
                    Phoenix.update PhoenixMsg innerMsg model.phxSocket
            in
            ( { model | phxSocket = updatedSocketModel }, newCommand )


toString json =
    Encode.encode 0 json



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Phoenix.listen PhoenixMsg model.phxSocket
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map viewMessage model.messages)
        , input [ onInput Input ] []
        , button [ onClick JoinChannel ] [ text "Join" ]
        , button [ onClick SendMessage ] [ text "send" ]
        , button [ onClick LeaveChannel ] [ text "Leave" ]
        , button [ onClick GetType ] [ text "get current type" ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
