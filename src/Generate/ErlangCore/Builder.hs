{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..)
  , Function(..)
  , functionsToText
  )
  where

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat, FPFormat(..))
import qualified Data.Text.Lazy as LazyText
import qualified Data.List as List



-- EXPRESSIONS


data Expr
  = Float Double
  | Int Int
  | Var Text
  | Apply Expr [Expr]
  | List [Expr]
  | Fun Text Expr
  | FunctionRef Text Int



-- TOP LEVEL


data Function
  -- We wrap all top-level values in an empty function.
  -- The Erlang compiler _might_ optimize this away...
  = Function Text Expr -- 'f'/0 = fun () -> ...


functionsToText :: [Function] -> LazyText.Text
functionsToText functions =
  toLazyText (mconcat (map fromFunction functions))


fromFunction :: Function -> Builder
fromFunction function =
  case function of
    Function name expr ->
      quoted name <> "/0 =\n  fun () ->\n    " <> fromExpr expr <> "\n"



-- EXPRESSIONS


fromExpr :: Expr -> Builder
fromExpr expression =
  case expression of
    Float n ->
      formatRealFloat Exponent (Just 20) n

    Var varName ->
      fromText varName

    Apply function args ->
      mconcat
        [ "apply " <> fromExpr function <> " ("
        , commaSep (map fromExpr args)
        , ")"
        ]

    Int n ->
      decimal n

    List exprs ->
      "[" <> commaSep (map fromExpr exprs) <> "]"

    Fun arg body ->
      mconcat
        [ "fun (" <> fromText arg <> ") -> "
        , fromExpr body
        ]

    FunctionRef name airity ->
      quoted name <> "/" <> decimal airity


commaSep :: [Builder] -> Builder
commaSep builders =
  mconcat (List.intersperse ", " builders)


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"
