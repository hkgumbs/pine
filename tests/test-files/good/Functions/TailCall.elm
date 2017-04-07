module Main exposing (..)


infiniteCountDown n x =
    case n of
        0 ->
            0

        _ ->
            infiniteCountDown n x
