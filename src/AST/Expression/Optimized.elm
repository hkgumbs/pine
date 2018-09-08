module AST.Expression.Optimized exposing
    ( Choice(..)
    , Decider(..)
    , Decl(..)
    , Def(..)
    , Expr(..)
    , Main(..)
    )

import AST.Effects as Effects
import AST.Literal as Literal
import AST.Module.Name as ModuleName
import AST.Variable as Var
import GenericSet exposing (GenericSet)
import Optimize.DecisionTree as DT
import Reporting.Region as R



-- TOP LEVEL DECLARATIONS


type Decl
    = Decl
        { direct : GenericSet Var.Global
        , indirect : GenericSet Var.Global
        , effects : Maybe Effects.ManagerType
        , body : Def
        }



-- DEFINITIONS


type Def
    = Def Expr
    | TailDef (List String) Expr



-- EXPRESSIONS


type Expr
    = Literal Literal.Literal
    | VarLocal String
    | VarGlobal Var.Global
    | List (List Expr)
    | Binop Var.Global Expr Expr
    | Function (List String) Expr
    | Call Expr (List Expr)
    | TailCall String (List String) (List Expr)
    | If (List ( Expr, Expr )) Expr
    | Let (List ( String, Def )) Expr
    | Case String (Decider Choice) (List ( Int, Expr ))
    | Ctor String (List Expr)
    | CtorAccess Expr Int
    | Access Expr String
    | Update Expr (List ( String, Expr ))
    | Record (List ( String, Expr ))
    | Cmd ModuleName.Canonical Effects.ManagerType
    | Sub ModuleName.Canonical Effects.ManagerType
    | OutgoingPort String Expr
    | IncomingPort String Expr
    | Program Main Expr
    | GLShader String
    | Crash ModuleName.Canonical R.Region (Maybe Expr)


type Main
    = VDom
    | NoFlags
    | Flags Expr


type Decider a
    = Leaf a
    | Chain
        { testChain : List ( DT.Path, DT.Test )
        , success : Decider a
        , failure : Decider a
        }
    | FanOut
        { path : DT.Path
        , tests : List ( DT.Test, Decider a )
        , fallback : Decider a
        }


type Choice
    = Inline Expr
    | Jump Int
