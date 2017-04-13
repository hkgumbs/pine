module Generate.CoreErlang.Function
  ( topLevel, reference, anonymous
  , apply, nativeCall
  ) where

import Control.Monad (foldM)
import qualified Control.Monad.State as State

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Elm.Compiler.Module as Module
import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.Substitution as Subst


topLevel :: Module.Canonical -> Text -> Core.Expr -> Core.Function
topLevel moduleName name =
  Core.Function (Module.qualifiedVar moduleName name) []


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.LFunction (Module.qualifiedVar moduleName name) 0) []


anonymous :: [Text] -> Core.Expr -> Core.Expr
anonymous args body =
  foldr (\a -> Core.Fun [a]) body args


apply :: Core.Expr -> [Core.Expr] -> State.State Int Core.Expr
apply =
  foldM $ Subst.two (\f a -> Core.Apply f [a])


nativeCall
  :: Module.Canonical
  -> Text
  -> [Core.Expr]
  -> State.State Int Core.Expr
nativeCall (Module.Canonical _ rawModule) name =
  Subst.many (Core.Call (Text.drop 7 rawModule) name)
