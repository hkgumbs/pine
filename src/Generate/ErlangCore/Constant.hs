{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Constant
  ( ctor, literal
  ) where

import Data.Text (Text)

import qualified AST.Literal as Literal

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.String as String


ctor :: Text -> [Core.Constant] -> Core.Constant
ctor name args =
  Core.Tuple $ Core.Atom name : args


literal :: Literal.Literal -> Core.Constant
literal literal =
  case literal of
    Literal.Chr c ->
      String.character c

    Literal.Str text ->
      String.bitString text

    Literal.IntNum n ->
      Core.Int n

    Literal.FloatNum n ->
      Core.Float n

    Literal.Boolean b ->
      Core.Atom $ if b then "true" else "false"
