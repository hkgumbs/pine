module Generate.ErlangCore.Function
  (makeTopLevel, reference, externalCall
  ) where


import Data.Text (Text)

import Generate.ErlangCore.Builder as Core
import Elm.Compiler.Module as Module



makeTopLevel :: Module.Canonical -> Text -> Core.Expr -> Core.Function
makeTopLevel moduleName name =
  -- We wrap all top-level values in an no-arg function.
  Core.Function (Module.qualifiedVar moduleName name) []


reference :: Module.Canonical -> Text -> Core.Expr
reference moduleName name =
  Core.Apply (Core.FunctionRef (Module.qualifiedVar moduleName name) 0) []


externalCall :: Module.Canonical -> Text -> [Core.Expr] -> Core.Expr
externalCall =
  Core.Call . Module.moduleToText
