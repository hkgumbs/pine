module Reporting.Bag exposing
    ( Bag
    , append
    , cons
    , empty
    , fromList
    , map
    , singleton
    , toList
    )

-- BAGS


type Bag a
    = Empty
    | One a
    | Two (Bag a) (Bag a)
    | Many (List a)


empty : Bag a
empty =
    Empty


singleton : a -> Bag a
singleton =
    One


cons : a -> Bag a -> Bag a
cons x bag =
    case bag of
        Empty ->
            One x

        _ ->
            Two (One x) bag


append : Bag a -> Bag a -> Bag a
append left right =
    case ( left, right ) of
        ( other, Empty ) ->
            other

        ( Empty, other ) ->
            other

        ( _, _ ) ->
            Two left right


map : (a -> b) -> Bag a -> Bag b
map func bag =
    case bag of
        Empty ->
            Empty

        One x ->
            One (func x)

        Two left right ->
            Two (map func left) (map func right)

        Many xs ->
            Many (List.map func xs)



-- LIST CONVERSIONS


fromList : List a -> Bag a
fromList =
    Many


toList : (a -> b) -> Bag a -> List b
toList func bag =
    toListHelp func bag []


toListHelp : (a -> b) -> Bag a -> List b -> List b
toListHelp func bag list =
    case bag of
        Empty ->
            list

        One x ->
            func x :: list

        Two left right ->
            toListHelp func left (toListHelp func right list)

        Many xs ->
            List.foldr (\x newList -> func x :: newList) list xs
