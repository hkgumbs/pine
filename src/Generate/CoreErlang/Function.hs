module Generate.CoreErlang.Function
  ( topLevel, reference, apply, anonymous
  ) where

import Data.Text (Text)

import qualified Elm.Compiler.Module as Module
import qualified Generate.CoreErlang.Builder as Core


topLevel :: Module.Canonical -> Text -> Core.Expr -> Core.Function
topLevel moduleName name =
  Core.Function (Module.qualifiedVar moduleName name) []


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.LFunction (Module.qualifiedVar moduleName name) 0) []


apply :: Core.Literal -> Core.Literal -> Core.Expr
apply var argument =
  Core.Apply var [argument]


anonymous :: [Text] -> Core.Expr -> Core.Expr
anonymous args body =
  foldr (\a -> Core.Fun [a]) body args
