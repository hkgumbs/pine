{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified Generate.ErlangCore.Builder as Core
import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified Reporting.Annotation as Annotation

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Function as Function
import qualified Generate.ErlangCore.String as String


generate :: Module.Module (Module.Info [Can.Def]) -> LazyText.Text
generate (Module.Module name _ info) =
  let
    generateDef (Can.Def _region pattern body _maybeType) =
      Function.topLevel name pattern (generateExpr body)
  in
    Core.functionsToText $ map generateDef (Module.program info)


generateExpr :: Can.Expr -> Core.Expr
generateExpr (Annotation.A _region canonical) =
  case canonical of
    Can.Literal literal ->
      generateLiteral literal

    Can.Var var ->
      generateVar var

    Can.List exprs ->
      Core.List (map generateExpr exprs)

    Can.Lambda pattern body ->
      Function.lambda pattern (generateExpr body)

    Can.App function arg ->
      Function.app generateExpr function arg

    Can.Ctor (Var.Canonical _home name) exprs ->
      Core.Tuple (Core.Atom name : map generateExpr exprs)

    Can.Program _main expr ->
      generateExpr expr


generateLiteral :: Literal.Literal -> Core.Expr
generateLiteral literal =
  case literal of
    Literal.FloatNum n ->
      Core.Float n

    Literal.IntNum n ->
      Core.Int n

    Literal.Chr c ->
      String.character c

    Literal.Str text ->
      String.bitString text


generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  case home of
    Var.Local ->
      Core.Var name

    Var.Module moduleName ->
      Function.reference moduleName name

    Var.TopLevel moduleName ->
      Function.reference moduleName name
