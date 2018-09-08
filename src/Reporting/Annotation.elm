module Reporting.Annotation exposing
    ( Annotated(..)
    , Commented
    , Located
    , at
    , drop
    , listToDict
    , map
    , merge
    , sameAs
    )

import Dict exposing (Dict)
import Prelude exposing (maybe)
import Reporting.Region as R



-- ANNOTATION


type Annotated annotation a
    = A annotation a


type alias Located a =
    Annotated R.Region a


type alias Commented a =
    Annotated ( R.Region, Maybe String ) a



-- CREATE


at : R.Position -> R.Position -> a -> Located a
at start end value =
    A (R.Region start end) value


merge : Located a -> Located b -> value -> Located value
merge (A region1 _) (A region2 _) value =
    A (R.merge region1 region2) value


sameAs : Annotated info a -> b -> Annotated info b
sameAs (A annotation _) value =
    A annotation value



-- MANIPULATE


map : (a -> b) -> Annotated info a -> Annotated info b
map f (A info value) =
    A info (f value)


drop : Annotated info a -> a
drop (A _ value) =
    value



-- ANALYZE


listToDict : (a -> comparable) -> List (Annotated i a) -> Dict comparable (List i)
listToDict toKey list =
    let
        maybeCons value =
            Just << maybe [] ((::) value)

        add (A info value) dict =
            Dict.update (toKey value) (maybeCons info) dict
    in
    List.foldl add Dict.empty list
