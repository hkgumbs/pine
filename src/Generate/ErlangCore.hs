{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Generate.ErlangCore.Builder as Core
import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified AST.Expression.Canonical as Can
import qualified AST.Pattern as Pattern
import qualified Reporting.Annotation as Annotation

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Function as Function
import qualified Generate.ErlangCore.String as String


generate :: Module.Module (Module.Info [Can.Def]) -> LazyText.Text
generate (Module.Module name _path info) =
  let
    generateDef (Can.Def _region pattern body _maybeType) =
      Function.topLevel name pattern (generateExpr body)
  in
    Core.functionsToText $ map generateDef (Module.program info)


generateExpr :: Can.Expr -> Core.Expr
generateExpr expr =
  case Annotation.drop expr of
    Can.Literal literal ->
      Core.Lit $ generateLiteral literal

    Can.Var var ->
      generateVar var

    Can.List exprs ->
      Core.Lit . Core.List $ map (Core.NonTerm . generateExpr) exprs

    Can.Lambda pattern body ->
      Function.lambda pattern (generateExpr body)

    Can.App f arg ->
      generateApp f arg

    Can.Ctor var exprs ->
      Core.Lit $ generateCtor var (map (Core.NonTerm . generateExpr) exprs)

    Can.Case expr clauses ->
      Core.Case (generateExpr expr) (map generateClause clauses)

    Can.Program _main expr ->
      generateExpr expr


generateLiteral :: Literal.Literal -> Core.Literal a
generateLiteral literal =
  case literal of
    Literal.FloatNum n ->
      Core.Term $ Core.Float n

    Literal.IntNum n ->
      Core.Term $ Core.Int n

    Literal.Chr c ->
      String.character c

    Literal.Str text ->
      String.bitString text


generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  case home of
    Var.Local ->
      Core.Lit . Core.Term $ Core.Var name

    Var.Module moduleName ->
      Function.reference moduleName name

    Var.TopLevel moduleName ->
      Function.reference moduleName name


generateApp :: Can.Expr -> Can.Expr -> Core.Expr
generateApp f arg =
  let
    splitFunction apps =
      (head apps, map generateExpr (tail apps ++ [arg]))

    (function, generatedArgs) =
      splitFunction (Can.collectApps f)
  in
    case Annotation.drop function of
      Can.Var (Var.Canonical (Var.Module moduleName) name)
        | ModuleName.canonicalIsNative moduleName ->
        Function.nativeCall moduleName name generatedArgs

      _ ->
        Function.internalCall (generateExpr function) generatedArgs


generateClause :: (Pattern.Canonical, Can.Expr) -> Core.Clause
generateClause (pattern, expr) =
  let
    noOpGuard =
      (Core.Lit . Core.Term . Core.Atom) "true"
  in
    Core.Clause (generatePattern pattern) noOpGuard (generateExpr expr)


generatePattern :: Pattern.Canonical -> Core.Literal Core.Terminal
generatePattern pattern =
  case Annotation.drop pattern of
    Pattern.Anything ->
      Core.Term Core.Anything

    Pattern.Var name ->
      Core.Term $ Core.Var name

    Pattern.Literal literal ->
      generateLiteral literal

    Pattern.Ctor var args ->
      generateCtor var (map generatePattern args)


generateCtor :: Var.Canonical -> [Core.Literal a] -> Core.Literal a
generateCtor var args | Var.isTuple var =
  Core.Tuple args

generateCtor (Var.Canonical _home name) args =
  Core.Tuple $ (Core.Term . Core.Atom) name : args
