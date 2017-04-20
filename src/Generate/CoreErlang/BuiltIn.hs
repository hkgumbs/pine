{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang.BuiltIn
  ( get, update, element, effect, apply
  ) where

import Data.Text as Text

import qualified AST.Module.Name as ModuleName
import Generate.CoreErlang.Builder as Core


-- These should all move to Native.Utils once it is ready


get :: Text.Text -> Core.Literal -> Core.Expr
get key coreMap =
  Core.Call "maps" "get" [Core.LTerm (Core.Atom key), coreMap]


update :: [Core.Literal] -> Core.Expr
update =
  Core.Call "maps" "merge"


element :: Int -> Core.Literal -> Core.Expr
element i tuple =
  Core.Call "erlang" "element" [Core.LTerm (Core.Int i), tuple]


effect :: ModuleName.Canonical -> Core.Expr
effect moduleName =
  Core.Call "Platform" "leaf"
    [Core.LTerm (Core.Atom (ModuleName.canonicalToText moduleName))]


apply :: Core.Literal -> [Core.Literal] -> Core.Expr
apply function args =
  Core.Call "Runtime" "apply" (function : args)
