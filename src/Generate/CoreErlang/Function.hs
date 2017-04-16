module Generate.CoreErlang.Function
  ( topLevel, anonymous
  , apply, binop, reference
  ) where

import qualified Control.Monad.State as State

import Data.Text (Text)

import qualified AST.Variable as Var
import qualified AST.Helpers as Help
import qualified Elm.Compiler.Module as Module
import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.BuiltIn as BuiltIn
import qualified Generate.CoreErlang.Substitution as Subst


-- CREATING


topLevel :: Module.Canonical -> Text -> [Text] -> Core.Expr -> Core.Function
topLevel moduleName name args body =
  if Help.isOp name then
    function args body

  else
    function [] (anonymous args body)

  where
    function =
      Core.Function (Module.qualifiedVar moduleName name)


anonymous :: [Text] -> Core.Expr -> Core.Expr
anonymous args body =
  if null args then
    body

  else
    Core.Fun args body



-- USING


apply :: Core.Expr -> [Core.Expr] -> State.State Int Core.Expr
apply function args =
  if null args then
    return function

  else
    Subst.many1 BuiltIn.apply function args


binop :: Var.Canonical -> [Core.Expr] -> State.State Int Core.Expr
binop (Var.Canonical home name) =
  Subst.many (Core.Apply (Core.LFunction qualified 2))

  where
    qualified =
      case home of
        Var.Local -> error "Will go away when merged with upstream dev"
        Var.Module moduleName -> Module.qualifiedVar moduleName name
        Var.TopLevel moduleName -> Module.qualifiedVar moduleName name
        Var.BuiltIn -> error "Will go away when merged with upstream dev"


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.LFunction (Module.qualifiedVar moduleName name) 0) []
