{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..), Literal(..), Terminal(..), Clause(..)
  , Function(..)
  , functionsToText
  )
  where

import Prelude hiding (break)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (formatRealFloat, FPFormat(..))
import qualified Data.Text.Lazy as LazyText
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Char as Char



-- EXPRESSIONS


data Expr
  = Lit (Literal Expr)
  | Apply Expr [Expr]
  | Call Text Text [Expr]
  | Case Expr [Clause]
  | Fun [Text] Expr
  | FunctionRef Text Int


data Literal a
  = Term Terminal
  | NonTerm a
  | Tuple [Literal a]
  | List [Literal a]


data Terminal
  = Int Int
  | Char Char
  | Float Double
  | Atom Text
  | Var Text
  | Anything
  | BitString ByteString


data Clause =
  Clause
    { _pattern :: Literal Terminal
    , _guard :: Expr
    , _body :: Expr
    }


-- TOP LEVEL


data Function
  = Function Text [Text] Expr -- 'f'/0 = fun () -> ...


functionsToText :: [Function] -> LazyText.Text
functionsToText functions =
  toLazyText (mconcat (map fromFunction functions))


fromFunction :: Function -> Builder
fromFunction function =
  case function of
    Function name args body ->
      fromFunctionName name (length args) <> " = "
      <> fromFun args break (fromExpr body) <> "\n"



-- EXPRESSIONS


fromExpr :: Expr -> Builder
fromExpr expression =
  case expression of
    Lit lit ->
      fromLiteral fromExpr lit

    Apply function args ->
      "apply " <> fromExpr function <> " ("
      <> commaSep fromExpr args
      <> ")"

    Call moduleName functionName args ->
      "call " <> quoted moduleName <> ":" <> quoted functionName <> " ("
      <> commaSep fromExpr args
      <> ")"

    Case expr clauses ->
      let
        clause (Clause pattern guard body) =
          break <> "<" <> fromLiteral fromTerminal pattern
          <> "> when " <> fromExpr guard
          <> " -> " <> fromExpr body
      in
        "case " <> fromExpr expr <> " of" <> mconcat (map clause clauses)
        <> break <> "end"

    Fun args body ->
      fromFun args " " (fromExpr body)

    FunctionRef name airity ->
      fromFunctionName name airity


fromLiteral :: (a -> Builder) -> Literal a -> Builder
fromLiteral buildInner literal =
  case literal of
    Term term ->
      fromTerminal term

    NonTerm nonterm ->
      buildInner nonterm

    Tuple inner ->
      "{" <> commaSep (fromLiteral buildInner) inner <> "}"

    List inner ->
      "[" <> commaSep (fromLiteral buildInner) inner <> "]"


fromTerminal :: Terminal -> Builder
fromTerminal term =
  case term of
    Int n ->
      decimal n

    Char c ->
      decimal (Char.ord c)

    Float n ->
      formatRealFloat Exponent (Just 20) n

    Atom name ->
      quoted name

    Var name ->
      safeVar name

    Anything ->
      "_"

    BitString str ->
      let
        collectWord c rest =
          "#<" <> fromString (show c)
          <> ">(8,1,'integer',['unsigned'|['big']])" : rest
      in
        "#{" <> commaSep id (ByteString.foldr collectWord [] str) <> "}#"


fromFunctionName :: Text -> Int -> Builder
fromFunctionName name airity =
  quoted name <> "/" <> decimal airity


fromFun :: [Text] -> Builder -> Builder -> Builder
fromFun args separator body =
  "fun (" <> commaSep safeVar args <> ") ->" <> separator <> body



-- HELPERS


commaSep :: (a -> Builder) -> [a] -> Builder
commaSep toBuilder as =
  mconcat (List.intersperse ", " (map toBuilder as))


safeVar :: Text -> Builder
safeVar name =
  "_" <> fromText name


quoted :: Text -> Builder
quoted str =
  "'" <> fromText str <> "'"


break :: Builder
break =
  "\n\t"
