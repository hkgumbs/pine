module Generate.CoreErlang.Function
  ( topLevel, reference, anonymous
  , apply, binop, nativeCall
  ) where

import Control.Monad (foldM)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text

import qualified AST.Variable as Var
import qualified AST.Helpers as Help
import qualified Elm.Compiler.Module as Module
import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.Substitution as Subst


-- CONTRUCTING


topLevel :: Module.Canonical -> Text -> [Text] -> Core.Expr -> Core.Function
topLevel moduleName name args body =
  if Help.isOp name then
    function args body

  else
    function [] (anonymous args body)

  where
    function =
      Core.Function (Module.qualifiedVar moduleName name)


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.LFunction (Module.qualifiedVar moduleName name) 0) []


anonymous :: [Text] -> Core.Expr -> Core.Expr
anonymous args body =
  foldr (\a -> Core.Fun [a]) body args



-- USING


apply :: Core.Expr -> [Core.Expr] -> State.State Int Core.Expr
apply =
  foldM $ Subst.two (\f a -> Core.Apply f [a])


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


nativeCall
  :: Module.Canonical
  -> Text
  -> [Core.Expr]
  -> State.State Int Core.Expr
nativeCall (Module.Canonical _ rawModule) name =
  Subst.many (Core.Call (Text.drop 7 rawModule) name)
