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
import qualified AST.Module.Name as ModuleName
import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.Substitution as Subst
import Elm.Compiler.Module (qualifiedVar)


-- CONTRUCTING


topLevel :: ModuleName.Canonical -> Text -> [Text] -> Core.Expr -> Core.Function
topLevel moduleName name args body =
  if Help.isOp name then
    function args body

  else
    function [] (anonymous args body)

  where
    function =
      Core.Function (qualifiedVar moduleName name)


reference :: ModuleName.Canonical -> Text -> Core.Expr
reference moduleName name =
  if ModuleName.canonicalIsNative moduleName then
    -- Since we short-circuit Call's, these are no-arg functions
    nativeCall moduleName name []

  else
    Core.Apply (Core.LFunction (qualifiedVar moduleName name) 0) []


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
        Var.Module moduleName -> qualifiedVar moduleName name
        Var.TopLevel moduleName -> qualifiedVar moduleName name
        Var.BuiltIn -> error "Will go away when merged with upstream dev"


nativeCall
  :: ModuleName.Canonical
  -> Text
  -> [Core.Expr]
  -> State.State Int Core.Expr
nativeCall (ModuleName.Canonical _ rawModule) name =
  Subst.many (Core.Call (Text.drop 7 rawModule) name)
