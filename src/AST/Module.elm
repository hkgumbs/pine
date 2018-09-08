module AST.Module exposing
    ( Aliases
    , Canonical
    , CanonicalUnion
    , DefaultImport
    , Header(..)
    , HeaderDecl(..)
    , ImportMethod(..)
    , Info(..)
    , Interface(..)
    , Interfaces
    , Module(..)
    , Optimized
    , Source
    , SourceInfo(..)
    , SourceSettings
    , SourceTag(..)
    , Types
    , UnionInfo
    , Unions
    , UserImport
    , Valid
    , ValidInfo(..)
    , defaultHeaderDecl
    , emptySettings
    , privatize
    , toInterface
    )

import AST.Declaration as Decl
import AST.Effects as Effects
import AST.Exposing as Exposing
import AST.Expression.Canonical as Canonical
import AST.Expression.Optimized as Optimized
import AST.Module.Name as Name
import AST.Type as Type
import AST.Variable as Var
import Dict
import Docs.AST as Docs
import Reporting.Annotation as A
import Reporting.Region as R



-- HEADERS FOR PARSING


{-| Basic info needed to identify modules and determine dependencies.
-}
type Header imports
    = Header (Maybe HeaderDecl) imports


type HeaderDecl
    = HeaderDecl
        { tag : SourceTag
        , name : Name.Raw
        , exports : Exposing.Raw
        , settings : SourceSettings
        , docs : A.Located (Maybe String)
        }


defaultHeaderDecl : HeaderDecl
defaultHeaderDecl =
    let
        zero =
            R.Position 1 1

        noDocs =
            A.at zero zero Nothing
    in
    HeaderDecl Normal "Main" Exposing.Open emptySettings noDocs



-- MODULES


type Module phase
    = Module Name.Canonical phase


type alias Source =
    Module SourceInfo


type SourceInfo
    = Source
        { srcTag : SourceTag
        , srcSettings : SourceSettings
        , srcDocs : A.Located (Maybe String)
        , srcExports : Exposing.Raw
        , srcImports : List UserImport
        , srcDecls : List Decl.Source
        }


type SourceTag
    = Normal
    | Effect R.Region
    | Port R.Region


type alias SourceSettings =
    A.Located (List ( A.Located String, A.Located String ))


emptySettings : SourceSettings
emptySettings =
    A.A (error "region of empty settings should not be needed") []


type alias Valid =
    Module ValidInfo


type ValidInfo
    = Valid
        { validDocs : A.Located (Maybe String)
        , validExports : Exposing.Raw
        , validImports : ( List DefaultImport, List UserImport )
        , validDecls : Decl.Valid
        , validEffects : Effects.Raw
        }


type Canonical
    = Module (Info Canonical.Expr)


type Optimized
    = Module (Info (List ( String, Optimized.Decl )))



-- IMPORTS


type alias UserImport =
    A.Located ( A.Located Name.Raw, ImportMethod )


type alias DefaultImport =
    ( Name.Raw, ImportMethod )


type ImportMethod
    = ImportMethod
        { alias_ : Maybe String
        , exposedVars : Exposing.Raw
        }



-- LATE PHASE MODULE INFORMATION


type Info program
    = Info
        { docs : A.Located (Maybe Docs.Centralized)
        , exports : Exposing.Canonical
        , imports : List Name.Raw
        , program : program
        , types : Types
        , fixities : List Decl.Infix
        , aliases : Aliases
        , unions : Unions
        , effects : Effects.Canonical
        }


type alias Types =
    Dict.Dict String Type.Canonical


type alias Aliases =
    Dict.Dict String ( List String, Type.Canonical )


type alias Unions =
    Dict.Dict String (UnionInfo String)


type alias UnionInfo v =
    ( List String, List ( v, List Type.Canonical ) )


type alias CanonicalUnion =
    ( Var.Canonical, UnionInfo Var.Canonical )



-- INTERFACES


type alias Interfaces =
    Dict.Dict Name.Canonical Interface


{-| Key facts about a module, used when reading info from .elmi files.
-}
type Interface
    = Interface
        { iExports : Exposing.Canonical
        , iImports : List Name.Raw -- TODO perhaps use this to crawl faster
        , iTypes : Types
        , iUnions : Unions
        , iAliases : Aliases
        , iFixities : List Decl.Infix
        }


toInterface : Optimized -> Interface
toInterface (Module _ myInfo) =
    Interface
        { iExports = exports myInfo
        , iImports = imports myInfo
        , iTypes = types myInfo
        , iUnions = unions myInfo
        , iAliases = aliases myInfo
        , iFixities = fixities myInfo
        }


privatize : Interface -> Maybe Interface
privatize (Interface _ _ _ myUnions myAliases _) =
    if Dict.null myUnions && Dict.null myAliases then
        Nothing

    else
        Just <|
            Interface
                { iExports = Exposing.nothing
                , iImports = []
                , iTypes = Dict.empty
                , iUnions = myUnions
                , iAliases = myAliases
                , iFixities = []
                }
