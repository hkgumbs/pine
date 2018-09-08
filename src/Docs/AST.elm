module Docs.AST exposing
    ( Alias(..)
    , Centralized
    , Checked
    , Docs(..)
    , Entry(..)
    , Good
    , GoodValue
    , Raw
    , RawValue
    , Union(..)
    , Value(..)
    )

import AST.Declaration as Decl
import Elm.Compiler.Type as Type
import Reporting.Annotation as A



-- DOCS


type Docs unions aliases values
    = Docs
        { overview : String
        , unions : Dict.Dict String unions
        , aliases : Dict.Dict String aliases
        , values : Dict.Dict String values
        }


type alias Centralized =
    Docs (Raw Union) (Raw Alias) (Raw RawValue)


type alias Checked =
    Docs (Good Union) (Good Alias) (Good GoodValue)



-- ENTRY


type Entry comment details
    = Entry
        { comment : comment
        , details : details
        }


type alias Raw a =
    A.Located (Entry (Maybe String) a)


type alias Good a =
    Entry String a



-- INFO


type Alias
    = Alias (List String) Type.Type


type Union
    = Union (List String) (List ( String, List Type.Type ))


type Value tipe
    = Value tipe
    | Infix tipe Decl.Assoc Int


type alias RawValue =
    Value (Maybe Type.Type)


type alias GoodValue =
    Value Type.Type
