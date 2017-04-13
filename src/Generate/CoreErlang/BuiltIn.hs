{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang.BuiltIn
  ( get, element, effect
  ) where

import Data.Text as Text

import qualified AST.Module.Name as ModuleName
import Generate.CoreErlang.Builder as Core


-- These should all move to Native.Utils once it is ready


get :: Text.Text -> Core.Literal -> Core.Expr
get key coreMap =
  Core.Call "maps" "get" [Core.LTerm (Core.Atom key), coreMap]


element :: Int -> Core.Literal -> Core.Expr
element i tuple =
  Core.Call "erlang" "element" [Core.LTerm (Core.Int i), tuple]


effect :: ModuleName.Canonical -> Core.Expr
effect moduleName =
  Core.Fun ["value"]
    $ Core.Lit
    $ Core.LTuple
        [ Core.LTerm (Core.Atom "leaf")
        , Core.LTerm (Core.Atom (ModuleName.canonicalToText moduleName))
        , Core.LTerm (Core.Var "value")
        ]
