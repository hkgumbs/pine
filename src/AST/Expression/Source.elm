module AST.Expression.Source exposing
    ( Expr
    , Expr_(..)
    , RawDef
    , RawDef_(..)
    , RawExpr
    , RawExpr_
    , ValidDef(..)
    , ValidExpr
    , ValidExpr_
    , collectLambdas
    , getPattern
    , tuple
    , var
    , zero
    )

import AST.Helpers as Help
import AST.Literal as Literal
import AST.Pattern as Ptrn
import AST.Type as Type
import AST.Variable as Var
import Reporting.Annotation as A
import Reporting.Region as R



-- EXPRESSIONS


type alias Expr def =
    A.Located (Expr_ def)


type Expr_ def
    = Literal Literal.Literal
    | Var Var.Raw
    | List (List (Expr def))
    | Binop (List ( Expr def, A.Located String )) (Expr def)
    | Lambda Ptrn.Raw (Expr def)
    | App (Expr def) (Expr def)
    | If (List ( Expr def, Expr def )) (Expr def)
    | Let (List def) (Expr def)
    | Case (Expr def) (List ( Ptrn.Raw, Expr def ))
    | Ctor String (List (Expr def))
    | Access (Expr def) String
    | Update (Expr def) (List ( A.Located String, Expr def ))
    | Record (List ( A.Located String, Expr def ))
    | GLShader String String Literal.Shader


type alias RawExpr =
    Expr RawDef


type alias RawExpr_ =
    Expr_ RawDef


type alias ValidExpr =
    Expr ValidDef


type alias ValidExpr_ =
    Expr_ ValidDef



-- DEFINITIONS


type alias RawDef =
    A.Located RawDef_


type RawDef_
    = Definition Ptrn.Raw RawExpr
    | Annotation String Type.Raw


type ValidDef
    = Def R.Region Ptrn.Raw ValidExpr (Maybe Type.Raw)



-- HELPERS


var : String -> Expr_ def
var x =
    Var (Var.Raw x)


tuple : List (Expr def) -> Expr_ def
tuple expressions =
    Ctor (Help.makeTuple (List.length expressions)) expressions


zero : Expr_ def
zero =
    Literal (Literal.IntNum 0)


getPattern : ValidDef -> Ptrn.Raw
getPattern (Def _ pattern _ _) =
    pattern


collectLambdas : Expr def -> ( List Ptrn.Raw, Expr def )
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
