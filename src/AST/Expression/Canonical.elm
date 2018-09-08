module AST.Expression.Canonical exposing
    ( Def(..)
    , Expr
    , Expr_(..)
    , Main(..)
    , SortedDefs(..)
    , collectApps
    , collectFields
    , collectLambdas
    , localVar
    , toSortedDefs
    )

import AST.Effects as Effects
import AST.Literal as Literal
import AST.Module.Name as ModuleName
import AST.Pattern as Ptrn
import AST.Type as Type
import AST.Variable as Var
import Reporting.Annotation as A
import Reporting.Region as R



-- EXPRESSIONS


type alias Expr =
    A.Annotated R.Region Expr_


type Expr_
    = Literal Literal.Literal
    | Var Var.Canonical
    | List (List Expr)
    | Binop Var.Canonical Expr Expr
    | Lambda Ptrn.Canonical Expr
    | App Expr Expr
    | If (List ( Expr, Expr )) Expr
    | Let (List Def) Expr
    | Case Expr (List ( Ptrn.Canonical, Expr ))
    | Ctor Var.Canonical (List Expr)
    | Access Expr String
    | Update Expr (List ( String, Expr ))
    | Record (List ( String, Expr ))
      -- for type checking and code gen only
    | Cmd ModuleName.Canonical Effects.ManagerType
    | Sub ModuleName.Canonical Effects.ManagerType
    | OutgoingPort String Type.Canonical
    | IncomingPort String Type.Canonical
    | Program Main Expr
    | SaveEnv ModuleName.Canonical Effects.Canonical
    | GLShader String String Literal.Shader


type Main
    = VDom
    | NoFlags
    | Flags Type.Canonical



-- DEFS


type Def
    = Def R.Region Ptrn.Canonical Expr (Maybe (A.Located Type.Canonical))



-- SORTED DEFS


type SortedDefs
    = NoMain (List Def)
    | YesMain (List Def) Def (List Def)


toSortedDefs : Expr -> SortedDefs
toSortedDefs (A.A _ expr) =
    case expr of
        Let defs body ->
            List.foldr defCons (toSortedDefs body) defs

        _ ->
            NoMain []


defCons : Def -> SortedDefs -> SortedDefs
defCons ((Def _ (A.A _ pattern) _ _) as def) sortedDefs =
    case ( pattern, sortedDefs ) of
        ( Ptrn.Var "main", NoMain defs ) ->
            YesMain [] def defs

        ( _, NoMain defs ) ->
            NoMain (def :: defs)

        ( _, YesMain defs main rest ) ->
            YesMain (def :: defs) main rest



-- HELPERS


localVar : String -> Expr_
localVar x =
    Var (Var.Canonical Var.Local x)



-- COLLECTORS


collectApps : Expr -> List Expr
collectApps ((A.A _ expr) as annExpr) =
    case expr of
        App a b ->
            collectApps a ++ [ b ]

        _ ->
            [ annExpr ]


collectFields : Expr -> Maybe String
collectFields expr =
    collectFieldsHelp expr ""


collectFieldsHelp : Expr -> String -> Maybe String
collectFieldsHelp (A.A _ expr) builder =
    case expr of
        Var var ->
            Just (Var.toString var ++ builder)

        Access record field ->
            collectFieldsHelp record ("." ++ field ++ builder)

        _ ->
            Nothing


collectLambdas : Expr -> ( List Ptrn.Canonical, Expr )
collectLambdas ((A.A _ expr) as lexpr) =
    case expr of
        Lambda pattern body ->
            let
                ( ps, body_ ) =
                    collectLambdas body
            in
            ( pattern :: ps, body_ )

        _ ->
            ( [], lexpr )
