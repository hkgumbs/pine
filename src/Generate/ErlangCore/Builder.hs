{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..)
  , Function(..)
  , functionsToText
  )
  where

import Data.Monoid ((<>))
import Data.Text (Text, head)
import Data.Char (ord)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat, FPFormat(..))
import qualified Data.Text.Lazy as LazyText
import qualified Data.List as List



-- EXPRESSIONS


data Expr
  = Float Double
  | Int Int
  | Char Text
  | Atom Text
  | Var Text
  | Apply Expr [Expr]
  | Call Text Text [Expr]
  | Tuple [Expr]
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
      quoted name <> "/0 = fun () ->\n\t" <> fromExpr expr <> "\n"



-- EXPRESSIONS


fromExpr :: Expr -> Builder
fromExpr expression =
  case expression of
    Float n ->
      formatRealFloat Exponent (Just 20) n

    Atom name ->
      quoted name

    Var name ->
      safeVar name

    Apply function args ->
      "apply " <> fromExpr function <> " ("
      <> commaSep fromExpr args
      <> ")"

    Call moduleName functionName args ->
      "call " <> quoted moduleName <> ":" <> quoted functionName <> " ("
      <> commaSep fromExpr args
      <> ")"

    Int n ->
      decimal n

    Char c ->
      decimal $ ord (Data.Text.head c)

    Tuple exprs ->
      "{" <> commaSep fromExpr exprs <> "}"

    List exprs ->
      "[" <> commaSep fromExpr exprs <> "]"

    Fun arg body ->
      "fun (" <> safeVar arg <> ") -> " <> fromExpr body

    FunctionRef name airity ->
      quoted name <> "/" <> decimal airity


commaSep :: (a -> Builder) -> [a] -> Builder
commaSep toBuilder as =
  mconcat (List.intersperse ", " (map toBuilder as))


safeVar :: Text -> Builder
safeVar name =
  "_" <> fromText name


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"
