{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Builder
  ( Expr(..), Literal(..), Ref(..), Constant(..)
  , Clause(..), Pattern(..)
  , Function(..)
  , encodeUtf8
  )
  where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text
import qualified Data.List as List
import qualified Data.Char as Char



-- AST


data Expr
  = C Literal
  | Apply Ref Text [Literal] -- apply 'f'/0 ()
  | Call Text Text [Literal] -- call 'module':'f' ()
  | Case Literal [Clause] -- case <_cor0> of ...
  | Let Text Expr Expr -- let <_cor0> = 23 in ...
  | LetRec Text [Text] Expr Expr -- letrec 'foo'/0 = fun () -> ... in ...
  | Fun [Text] Expr -- fun () -> ...


data Literal
  = Literal (Constant Literal)


data Ref
  = VarRef -- _f
  | FunctionRef -- 'f'/1


data Constant context
  = Int Int
  | Char Char
  | Float Double
  | Atom Text
  | Var Text
  | Anything
  | BitString ByteString
  | Tuple [context]
  | Cons context context
  | Nil


data Pattern
  = Pattern (Constant Pattern)
  | Alias Text Pattern


data Clause
  = Clause
    { _pattern :: Pattern
    , _guard :: Expr
    , _body :: Expr
    }


data Function
  = Function Text [Text] Expr -- 'f'/0 = fun () -> ...



-- TOP LEVEL


encodeUtf8 :: [Function] -> Builder
encodeUtf8 functions =
  mconcat (map fromFunction functions)


fromFunction :: Function -> Builder
fromFunction function =
  case function of
    Function name args body ->
      fromFunctionName name args <> " = "
      <> fromFun args "" body <> "\n"



-- EXPRESSIONS


fromExpr :: Builder -> Expr -> Builder
fromExpr indent expression =
  case expression of
    C literal ->
      fromLiteral literal

    Apply VarRef name args ->
      "apply " <> safeVar name
      <> " (" <> commaSep fromLiteral args <> ")"

    Apply FunctionRef name args ->
      "apply " <> fromFunctionName name args
      <> " (" <> commaSep fromLiteral args <> ")"

    Call moduleName functionName args ->
      "call " <> quoted moduleName <> ":" <> quoted functionName <> " ("
      <> commaSep fromLiteral args
      <> ")"

    Case switch clauses ->
      let
        clause (Clause pattern guard body) =
          "\n" <> deeper indent <> "<" <> fromPattern pattern
          <> "> when " <> fromExpr indent guard <> " ->\n"
          <> deeper (deeper indent) <> fromExpr (deeper (deeper indent)) body
      in
        "case " <> fromLiteral switch <> " of"
        <> mconcat (map clause clauses)
        <> "\n" <> indent <> "end"

    Let var binding body ->
      "let <" <> safeVar var <> "> =\n"
      <> deeper indent <> fromExpr (deeper indent) binding <> "\n"
      <> indent <> "in\n"
      <> deeper indent <> fromExpr (deeper indent) body

    LetRec name args binding body ->
      "letrec " <> fromFunctionName name args <> " =\n"
      <> deeper indent <> fromFun args (deeper indent) binding <> "\n"
      <> indent <> "in\n"
      <> deeper indent <> fromExpr (deeper indent) body

    Fun args body ->
      fromFun args indent body


fromLiteral :: Literal -> Builder
fromLiteral (Literal constant) =
  fromConstant fromLiteral constant


fromPattern :: Pattern -> Builder
fromPattern pattern =
  case pattern of
    Pattern constant ->
      fromConstant fromPattern constant

    Alias name p ->
      safeVar name <> " = " <> fromPattern p


fromConstant :: (a -> Builder) -> Constant a -> Builder
fromConstant buildContext constant =
  case constant of
    Int n ->
      intDec n

    Char c ->
      intDec (Char.ord c)

    Float n ->
      doubleDec n

    Atom name ->
      quoted name

    Var name ->
      safeVar name

    Anything ->
      "_"

    BitString str ->
      let
        collectWord w rest =
          "#<" <> word8Dec w <> ">(8,1,'integer',['unsigned'|['big']])" : rest
      in
        "#{" <> commaSep id (ByteString.foldr collectWord [] str) <> "}#"

    Tuple inners ->
      "{" <> commaSep buildContext inners <> "}"

    Cons first rest ->
      "[" <> buildContext first <> "|" <> buildContext rest <> "]"

    Nil ->
      "[]"


fromFunctionName :: Text -> [a] -> Builder
fromFunctionName name args =
  quoted name <> "/" <> intDec (length args)


fromFun :: [Text] -> Builder -> Expr -> Builder
fromFun args indent body =
  "fun (" <> commaSep safeVar args <> ") ->\n"
  <> deeper indent <> fromExpr (deeper indent) body



-- HELPERS


commaSep :: (a -> Builder) -> [a] -> Builder
commaSep toBuilder as =
  mconcat (List.intersperse ", " (map toBuilder as))


safeVar :: Text -> Builder
safeVar name =
  "_" <> Text.encodeUtf8Builder name


quoted :: Text -> Builder
quoted text =
  "'" <> Text.encodeUtf8Builder text <> "'"


deeper :: Builder -> Builder
deeper indent =
  indent <> "\t"
