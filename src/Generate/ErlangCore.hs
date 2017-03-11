{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Generate.ErlangCore.Builder as Core
import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified AST.Expression.Canonical as Can
import qualified AST.Pattern as Pattern
import qualified AST.Helpers as Helpers
import qualified Reporting.Annotation as Annotation
import Elm.Compiler.Module (moduleToText, qualifiedVar)

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.String as String


generate :: Module.Module (Module.Info [Can.Def]) -> LazyText.Text
generate (Module.Module name _path info) =
  Core.functionsToText $ map (generateDef name) (Module.program info)


generateDef :: ModuleName.Canonical -> Can.Def -> Core.Function
generateDef moduleName (Can.Def _region pattern body _maybeType) =
  case Annotation.drop pattern of
    Pattern.Var name | Helpers.isOp name ->
      uncurry (generateOp moduleName name) (Can.collectLambdas body)

    Pattern.Var name ->
      Core.Function (qualifiedVar moduleName name) [] (generateExpr body)


generateOp
  :: ModuleName.Canonical
  -> Text
  -> [Pattern.Canonical]
  -> Can.Expr
  -> Core.Function
generateOp moduleName name args body =
  let
    patternToText p =
      case Annotation.drop p of
        Pattern.Var text ->
          text
  in
    Core.Function
      (qualifiedVar moduleName name)
      (map patternToText args)
      (generateExpr body)


generateExpr :: Can.Expr -> Core.Expr
generateExpr expr =
  case Annotation.drop expr of
    Can.Literal literal ->
      Core.Lit (generateLiteral literal)

    Can.Var var ->
      generateVar var

    Can.List exprs ->
      Core.Lit . Core.List $ map generateExpr exprs

    Can.Binop var lhs rhs ->
      Core.Apply (generateVar var) [generateExpr lhs, generateExpr rhs]

    Can.Lambda pattern body ->
      generateLambda pattern (generateExpr body)

    Can.App f arg ->
      generateApp f arg

    Can.Ctor var exprs ->
      generateCtor Core.Lit var (map generateExpr exprs)

    Can.Case expr clauses ->
      Core.Case (generateExpr expr) $
        map (\(pat, body) -> generateClause pat (generateExpr body)) clauses

    Can.Program _main expr ->
      generateExpr expr


generateLiteral :: Literal.Literal -> Core.Literal a
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
  let
    reference moduleName =
      if Helpers.isOp name then
        Core.FunctionRef (qualifiedVar moduleName name) 2
      else
        Core.Apply (Core.FunctionRef (qualifiedVar moduleName name) 0) []
  in
    case home of
      Var.Local ->
        Core.Lit (Core.Var name)

      Var.Module moduleName ->
        reference moduleName

      Var.TopLevel moduleName ->
        reference moduleName


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
        Core.Call (moduleToText moduleName) name generatedArgs

      _ ->
        foldl (\f a -> Core.Apply f [a]) (generateExpr function) generatedArgs


generateLambda :: Pattern.Canonical -> Core.Expr -> Core.Expr
generateLambda pattern body =
  case Annotation.drop pattern of
    Pattern.Var name ->
      Core.Fun [name] body

    Pattern.Ctor _var _args ->
      Core.Fun ["tmp"] $
        Core.Case (Core.Lit (Core.Var "tmp")) [generateClause pattern body]


generateClause :: Pattern.Canonical -> Core.Expr -> Core.Clause
generateClause pattern expr =
  Core.Clause (generatePattern pattern) (Core.Lit (Core.Atom "true")) expr


generatePattern :: Pattern.Canonical -> Core.Pattern
generatePattern pattern =
  case Annotation.drop pattern of
    Pattern.Anything ->
      Core.Pattern Core.Anything

    Pattern.Var name ->
      Core.Pattern (Core.Var name)

    Pattern.Literal literal ->
      Core.Pattern (generateLiteral literal)

    Pattern.Ctor var args ->
      generateCtor Core.Pattern var (map generatePattern args)


generateCtor :: (Core.Literal a -> a) -> Var.Canonical -> [a] -> a
generateCtor toInner var =
  if Var.isTuple var then
    toInner . Core.Tuple
  else
    toInner . Core.Tuple . (:) (toInner (Core.Atom (Var._name var)))
