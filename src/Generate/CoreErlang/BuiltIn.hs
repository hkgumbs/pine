{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang.BuiltIn
  ( get, element
  ) where

import Data.Text as Text

import Generate.CoreErlang.Builder as Core


-- The generate step depends on some built-in Erlang functions.
-- All of those can be found in this module.


get :: Text.Text -> Core.Literal -> Core.Expr
get key coreMap =
  Core.Call "maps" "get" [Core.LTerm (Core.Atom key), coreMap]


element :: Int -> Core.Literal -> Core.Expr
element i tuple =
  Core.Call "erlang" "element" [Core.LTerm (Core.Int i), tuple]
