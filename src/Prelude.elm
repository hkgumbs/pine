module Prelude exposing (init, last, lookup, mapBoth, maybe, repeat)


lookup : a -> List ( a, b ) -> Maybe b
lookup key list =
    case list of
        [] ->
            Nothing

        ( a, b ) :: rest ->
            if key == a then
                Just b

            else
                lookup key rest


maybe : b -> (a -> b) -> Maybe a -> b
maybe default f data =
    case data of
        Just a ->
            f a

        Nothing ->
            default


init : List a -> List a
init list =
    List.take (List.length list - 1) list


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: xs ->
            last xs


repeat : a -> List b -> List a
repeat a list =
    List.repeat (List.length list) a


mapBoth : (a -> c) -> (b -> d) -> ( a, b ) -> ( c, d )
mapBoth f g ( a, b ) =
    ( f a, g b )
