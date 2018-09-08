module AST.Declaration exposing (Alias, Assoc(..), Canonical, CommentOr(..), Decls(..), Infix(..), Raw(..), Source, Type(..), Union, Valid, addAlias, addDef, addInfix, addUnion, assocToString, defaultAssociativity, defaultPrecedence)

import AST.Expression.Canonical as Canonical
import AST.Expression.Source as Src
import AST.Type as Type
import Reporting.Annotation as A



-- SOURCE DECLARATIONS


type alias Source =
    CommentOr (A.Located Raw)


type CommentOr a
    = Comment (A.Located String)
    | Whatever a


type Raw
    = Def Src.RawDef
    | Union (Union Type.Raw)
    | Alias (Alias Type.Raw)
    | Fixity Infix
    | Port String Type.Raw



-- STRUCTURED DECLARATIONS


type Decls def tipe
    = Decls
        { defs : List (A.Commented def)
        , unions : List (A.Commented (Union tipe))
        , aliases : List (A.Commented (Alias tipe))
        , infixes : List Infix
        }


type alias Valid =
    Decls Src.ValidDef Type.Raw


type alias Canonical =
    Decls Canonical.Def Type.Canonical


addDef : A.Commented d -> Decls d t -> Decls d t
addDef def (Decls decls) =
    Decls { decls | defs = def :: decls.defs }


addUnion : A.Commented (Union t) -> Decls d t -> Decls d t
addUnion union (Decls decls) =
    Decls { decls | unions = union :: decls.unions }


addAlias : A.Commented (Alias t) -> Decls d t -> Decls d t
addAlias alias_ (Decls decls) =
    Decls { decls | aliases = alias_ :: decls.aliases }


addInfix : Infix -> Decls d t -> Decls d t
addInfix fixity (Decls decls) =
    Decls { decls | infixes = fixity :: decls.infixes }



-- TYPE DECLARATIONS


type Type body
    = Type String (List String) body


type alias Union tipe =
    Type (List ( String, List tipe ))


type alias Alias tipe =
    Type tipe



-- INFIX STUFF


type Infix
    = Infix String Assoc Int


type Assoc
    = Left
    | Non
    | Right


defaultAssociativity : Assoc
defaultAssociativity =
    Left


defaultPrecedence : Int
defaultPrecedence =
    9


assocToString : Assoc -> String
assocToString assoc =
    case assoc of
        Left ->
            "left"

        Non ->
            "non"

        Right ->
            "right"
