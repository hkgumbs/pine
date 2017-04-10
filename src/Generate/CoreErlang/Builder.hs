{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang.Builder
  ( Function(..)
  , Expr(..), Literal(..), Pattern(..)
  , Term(..)
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


data Function
  = Function Text [Text] Expr -- 'f'/0 = fun () -> ...


data Expr
  = Lit Literal
  | Map [(Literal, Literal)] -- ~{ 'a' => 1 }~
  | Update [(Literal, Literal)] Literal -- ~{ 'a' := 1 | _map }~
  | Apply Literal [Literal] -- apply _f ()
  | Call Text Text [Literal] -- call 'module':'f' ()
  | Case Literal [(Pattern, Expr)] -- case <_cor0> of ...
  | Let Text Expr Expr -- let <_cor0> = 23 in ...
  | LetRec Text [Text] Expr Expr -- letrec 'foo'/0 = fun () -> ... in ...
  | Fun [Text] Expr -- fun () -> ...


data Literal
  = LTerm Term
  | LTuple [Literal]
  | LCons Literal Literal
  | LFunction Text Int


data Pattern
  = PTerm Term
  | PAlias Text Pattern
  | PMap [(Pattern, Pattern)]
  | PTuple [Pattern]
  | PCons Pattern Pattern


data Term
  = Int Int
  | Char Char
  | Float Double
  | Atom Text
  | Var Text
  | Anything
  | BitString ByteString
  | Nil



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


fromFunctionName :: Text -> [a] -> Builder
fromFunctionName name args =
  quoted name <> "/" <> intDec (length args)


fromFun :: [Text] -> Builder -> Expr -> Builder
fromFun args indent body =
  "fun (" <> commaSep safeVar args <> ") ->\n"
  <> deeper indent <> fromExpr (deeper indent) body



-- EXPRESSIONS


fromExpr :: Builder -> Expr -> Builder
fromExpr indent expression =
  case expression of
    Lit literal ->
      fromLiteral literal

    Map pairs ->
      let
        fromPair (key, value) =
          fromLiteral key <> " => " <> fromLiteral value
      in
        "~{" <> commaSep fromPair pairs <> "}~"

    Update pairs var ->
      let
        fromPair (key, value) =
          fromLiteral key <> " := " <> fromLiteral value
      in
        "~{" <> commaSep fromPair pairs <> " | " <> fromLiteral var <> "}~"

    Apply name args ->
      "apply " <> fromLiteral name
      <> " (" <> commaSep fromLiteral args <> ")"

    Call moduleName functionName args ->
      "call " <> quoted moduleName <> ":" <> quoted functionName <> " ("
      <> commaSep fromLiteral args
      <> ")"

    Case switch clauses ->
      let
        fromClause (pattern, body) =
          "\n" <> deeper indent <> "<" <> fromPattern pattern
          <> "> when 'true' ->\n"
          <> deeper (deeper indent) <> fromExpr (deeper (deeper indent)) body
      in
        "case " <> fromLiteral switch <> " of"
        <> mconcat (map fromClause clauses)
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
fromLiteral literal =
  case literal of
    LTerm term ->
      fromTerm term

    LTuple values ->
      "{" <> commaSep fromLiteral values <> "}"

    LCons first rest ->
      "[" <> fromLiteral first <> "|" <> fromLiteral rest <> "]"

    LFunction name airity ->
      fromFunctionName name (replicate airity ())


fromPattern :: Pattern -> Builder
fromPattern pattern =
  case pattern of
    PTerm constant ->
      fromTerm constant

    PAlias name p ->
      safeVar name <> " = " <> fromPattern p

    PMap pairs ->
      let
        fromPair (key, value) =
          fromPattern key <> " := " <> fromPattern value
      in
      "~{" <> commaSep fromPair pairs <> "}~"

    PTuple values ->
      "{" <> commaSep fromPattern values <> "}"

    PCons first rest ->
      "[" <> fromPattern first <> "|" <> fromPattern rest <> "]"


fromTerm :: Term -> Builder
fromTerm constant =
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

    Nil ->
      "[]"



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
