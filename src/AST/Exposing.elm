module AST.Exposing exposing
    ( Canonical(..)
    , Entry(..)
    , Exposing(..)
    , Raw
    , closed
    , getName
    , nothing
    )

import Reporting.Annotation as A



-- CANONICAL


type Canonical
    = Canonical
        { values : List String
        , aliases : List String
        , unions : List ( String, List String )
        }


nothing : Canonical
nothing =
    Canonical { values = [], aliases = [], unions = [] }



-- RAW


type alias Raw =
    Exposing Entry


type Exposing a
    = Open
    | Explicit (List (A.Located a))


closed : Exposing a
closed =
    Explicit []


type Entry
    = Lower String
    | Upper String (Maybe (Exposing String))


getName : Entry -> String
getName entry =
    case entry of
        Lower name ->
            name

        Upper name _ ->
            name
