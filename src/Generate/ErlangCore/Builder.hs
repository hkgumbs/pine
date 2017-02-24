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
import qualified Data.List as List
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
  | Apply Id
  | List [Expr]


newtype Id = Id Text



-- STATEMENTS


data Stmt
  = FunctionStmt Id [Id] Expr -- 'f'/3 = fun (x, y, z) -> ...



-- CONVERT TO LAZY TEXT


stmtsToText :: [Stmt] -> LazyText.Text
stmtsToText stmts =
  toLazyText (fromStmtBlock "" stmts)



-- HELPERS


deeper :: Builder -> Builder
deeper indent =
  "    " <> indent


commaSep :: [Builder] -> Builder
commaSep builders =
  mconcat (List.intersperse ", " builders)


airity :: [a] -> Builder
airity args =
  "/" <> decimal (length args)



-- STATEMENTS


fromStmtBlock :: Builder -> [Stmt] -> Builder
fromStmtBlock indent stmts =
  mconcat (map (fromStmt indent) stmts)


fromStmt :: Builder -> Stmt -> Builder
fromStmt indent statement =
  case statement of
    FunctionStmt (Id name) args expr ->
      mconcat
        [ indent <> quoted name <> airity args <> " =\n"
        , deeper indent <> "fun (" <> commaSep (map fromId args) <> ") ->\n"
        , deeper indent <> fromExpr expr <> "\n"
        ]



-- ID


fromId :: Id -> Builder
fromId (Id name) =
  fromText name



-- EXPRESSIONS


fromExpr :: Expr -> Builder
fromExpr expression =
  case expression of
    Float n ->
      formatRealFloat Exponent (Just 20) n

    Apply functionName ->
      "apply '" <> fromId functionName <> "'/0 ()"

    Int n ->
      decimal n

    List exprs ->
      let
        bracket [] =
          "[]"

        bracket [onlyOne] =
          "[" <> fromExpr onlyOne <> "]"

        bracket (first : rest) =
          "[" <> fromExpr first <> "|" <> bracket rest <> "]"
      in
        bracket exprs



-- STRINGS


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"
