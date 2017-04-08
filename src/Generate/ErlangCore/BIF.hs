{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.BIF
  ( get, element
  ) where

import Data.Text as Text

import Generate.ErlangCore.Builder as Core


-- The generate step depends on some built-in Erlang functions.
-- All of those can be found in this module.


get :: Text.Text -> Core.Literal -> Core.Expr
get key coreMap =
  Core.Call "maps" "get" [Core.Literal (Core.Atom key), coreMap]


element :: Int -> Core.Literal -> Core.Expr
element index tuple =
  Core.Call "erlang" "element" [Core.Literal (Core.Int index), tuple]
