module Main exposing (..)


length list =
    case list of
        [] ->
            0

        [ a ] ->
            1

        a :: rest ->
            2


firstWithDefault list default =
    case list of
        [] ->
            default

        a :: _ ->
            a
