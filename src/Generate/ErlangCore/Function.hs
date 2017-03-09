module Generate.ErlangCore.Function where


import Data.Text (Text)

import qualified AST.Pattern as Pattern
import qualified Elm.Compiler.Module as Module
import qualified Reporting.Annotation as Annotation

import qualified Generate.ErlangCore.Builder as Core



topLevel :: Module.Canonical -> Pattern.Canonical -> Core.Expr -> Core.Function
topLevel moduleName pattern =
  -- wrap all top-level values in a no-arg function.
  case Annotation.drop pattern of
    Pattern.Var name ->
      Core.Function (Module.qualifiedVar moduleName name) []


lambda :: Pattern.Canonical -> Core.Expr -> Core.Expr
lambda pattern =
  case Annotation.drop pattern of
    Pattern.Var name ->
      Core.Fun [name]


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.FunctionRef (Module.qualifiedVar moduleName name) 0) []


nativeCall :: Module.Canonical -> Text -> [Core.Expr] -> Core.Expr
nativeCall =
  Core.Call . Module.moduleToText


internalCall :: Core.Expr -> [Core.Expr] -> Core.Expr
internalCall =
  foldl (\f a -> Core.Apply f [a])
