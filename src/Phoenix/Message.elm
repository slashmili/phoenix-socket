module Phoenix.Message exposing(Msg, mapAll, none)

type Msg msg
    = NoOp
    | ExternalMsg msg
    | ChannelJoined String

mapAll : (Msg msg -> msg) -> Msg msg -> msg
mapAll fn internalMsg =
    case internalMsg of
        ExternalMsg msg ->
            msg

        _ ->
            fn internalMsg

none: Msg msg
none = NoOp
