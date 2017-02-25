{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..), Id(..)
  , Function(..)
  , functionsToText
  )
  where

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Prelude hiding (lines)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat, FPFormat(..))
import qualified Data.Text.Lazy as LazyText



-- EXPRESSIONS


data Expr
  = Float Double
  | Int Int
  | Var Id
  | Apply Id
  | List [Expr]
  | Fun Id Expr


newtype Id = Id Text



-- TOP LEVEL


data Function
  = Function Id Expr -- 'f'/0 = fun () -> ...


functionsToText :: [Function] -> LazyText.Text
functionsToText functions =
  toLazyText (mconcat (map (fromFunction "") functions))


deeper :: Builder -> Builder
deeper indent =
  "  " <> indent


fromFunction :: Builder -> Function -> Builder
fromFunction indent function =
  case function of
    Function (Id name) expr ->
      mconcat
        [ indent <> quoted name <> "/0 =\n"
        , deeper indent <> "fun () ->\n"
        , fromExpr (deeper $ deeper indent) expr <> "\n"
        ]



-- EXPRESSIONS


fromExpr :: Builder -> Expr -> Builder
fromExpr indent expression =
  case expression of
    Float n ->
      indent <> formatRealFloat Exponent (Just 20) n

    Var varName ->
      indent <> fromId varName

    Apply functionName ->
      indent <> "apply '" <> fromId functionName <> "'/0 ()"

    Int n ->
      indent <> decimal n

    List exprs ->
      let
        bracket [] =
          "[]"

        bracket [onlyOne] =
          "[" <> fromExpr "" onlyOne <> "]"

        bracket (first : rest) =
          "[" <> fromExpr "" first <> "|" <> bracket rest <> "]"
      in
        indent <> bracket exprs

    Fun arg body ->
      mconcat
        [ indent <> "fun (" <> fromId arg <> ") ->\n"
        , fromExpr (deeper indent) body
        ]


fromId :: Id -> Builder
fromId (Id name) =
  fromText name


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"
