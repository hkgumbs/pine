module Generate.ErlangCore.Function where


import Data.Text (Text)

import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified AST.Expression.Canonical as Can
import qualified AST.Module.Name as ModuleName
import qualified Elm.Compiler.Module as Module
import qualified Reporting.Annotation as Annotation

import qualified Generate.ErlangCore.Builder as Core



topLevel :: Module.Canonical -> Pattern.Canonical -> Core.Expr -> Core.Function
topLevel moduleName (Annotation.A _ pattern) =
  -- wrap all top-level values in a no-arg function.
  case pattern of
    Pattern.Var name ->
      Core.Function (Module.qualifiedVar moduleName name) []


lambda :: Pattern.Canonical -> Core.Expr -> Core.Expr
lambda (Annotation.A _ pattern) =
  case pattern of
    Pattern.Var name ->
      Core.Fun [name]


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.FunctionRef (Module.qualifiedVar moduleName name) 0) []


app :: (Can.Expr -> Core.Expr) -> Can.Expr -> Can.Expr -> Core.Expr
app generateExpr function arg =
  let
    appHelp expr@(Annotation.A _ canonical) args =
      case canonical of
        Can.App f a ->
          appHelp f (generateExpr a : args)

        Can.Var (Var.Canonical (Var.Module moduleName) name)
          | ModuleName.canonicalIsNative moduleName ->
          Core.Call (Module.moduleToText moduleName) name args

        _ ->
          foldl (\f a -> Core.Apply f [a]) (generateExpr expr) args
  in
    appHelp function [generateExpr arg]
