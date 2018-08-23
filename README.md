# Phoenix Socket
This library is an [Elm](http://elm-lang.org/) client for [Phoenix](https://github.com/phoenixframework/phoenix) [channels](https://hexdocs.pm/phoenix/channels.html).

This package is not ready to be used in production! However you can use it with Elm 0.19 right now, considering elm/Websocket is not out yet.


The reasons you can use this package in Elm 0.19

1. It doesn't use Effects Manager.
2. Initially it supports both Websocket and LongPool transport layer. With the lack of Websocket package, the code related to Websocket is removed and you can use this package today in your Elm 0.19 project.


Take a look at `example` directory for a detailed example.

## Credit
Most of the codes an ideas are borrowed from [fbonetti/elm-phoenix-socket](https://github.com/fbonetti/elm-phoenix-socket)


## Goals
* I have started this project merely for the sake learning Elm.
* Make it easier to work with [jamesmacaulay/elm-graphql](https://github.com/jamesmacaulay/elm-graphql)
