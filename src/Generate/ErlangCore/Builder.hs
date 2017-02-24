{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..), Id(..)
  , Stmt(..)
  , stmtsToText
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
  | Function Id Expr


newtype Id = Id Text



-- STATEMENTS


data Stmt
  = FunctionStmt Id Expr -- 'f'/0 = fun () -> ...



-- CONVERT TO LAZY TEXT


stmtsToText :: [Stmt] -> LazyText.Text
stmtsToText stmts =
  toLazyText (fromStmtBlock "" stmts)



-- HELPERS


deeper :: Builder -> Builder
deeper indent =
  "  " <> indent



-- STATEMENTS


fromStmtBlock :: Builder -> [Stmt] -> Builder
fromStmtBlock indent stmts =
  mconcat (map (fromStmt indent) stmts)


fromStmt :: Builder -> Stmt -> Builder
fromStmt indent statement =
  case statement of
    FunctionStmt (Id name) expr ->
      mconcat
        [ indent <> quoted name <> "/0 =\n"
        , deeper indent <> "fun () ->\n"
        , fromExpr (deeper $ deeper indent) expr <> "\n"
        ]



-- ID


fromId :: Id -> Builder
fromId (Id name) =
  fromText name



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

    Function arg body ->
      mconcat
        [ indent <> "fun (" <> fromId arg <> ") ->\n"
        , fromExpr (deeper indent) body
        ]



-- STRINGS


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"
