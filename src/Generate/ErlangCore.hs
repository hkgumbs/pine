{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Control.Monad.State as State
import qualified Data.Text.Lazy as LazyText
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
import qualified Generate.ErlangCore.Application as App


generate :: Module.Module (Module.Info [Can.Def]) -> LazyText.Text
generate (Module.Module name _path info) =
  Core.functionsToText $ map (generateDef name) (Module.program info)


generateDef :: ModuleName.Canonical -> Can.Def -> Core.Function
generateDef moduleName (Can.Def _region pattern body _maybeType) =
  case Annotation.drop pattern of
    Pattern.Var name | Helpers.isOp name ->
      uncurry (generateOp moduleName name) (Can.collectLambdas body)

    Pattern.Var name ->
      Core.Function (qualifiedVar moduleName name) [] $
        State.evalState (generateExpr body) 1


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
      (State.evalState (generateExpr body) 1)


generateExpr :: Can.Expr -> State.State Int Core.Expr
generateExpr expr =
  case Annotation.drop expr of
    Can.Literal literal ->
      return $ Core.Lit (generateLiteral literal)

    Can.Var var ->
      return $ generateVar var

    Can.List exprs ->
      foldr (generateCons Core.Lit) (Core.Lit Core.Nil)
        <$> mapM generateExpr exprs

    Can.Binop var lhs rhs ->
      do  left <- generateExpr lhs
          right <- generateExpr rhs
          App.reduce3 (generateVar var) left right

    Can.Lambda pattern body ->
      generateLambda pattern <$> generateExpr body

    Can.App f arg ->
      generateApp f arg

    Can.Ctor var exprs ->
      generateCtor Core.Lit var <$> mapM generateExpr exprs

    Can.Case expr clauses ->
      do  switch <- generateExpr expr
          Core.Case switch <$>
            mapM (\(pat, body) -> generateClause pat <$> generateExpr body) clauses

    Can.Program _main expr ->
      generateExpr expr


generateLiteral :: Literal.Literal -> Core.Literal a
generateLiteral literal =
  case literal of
    Literal.Chr c ->
      String.character c

    Literal.Str text ->
      String.bitString text

    Literal.IntNum n ->
      Core.Int n

    Literal.FloatNum n ->
      Core.Float n

    Literal.Boolean b ->
      Core.Atom $ if b then "true" else "false"


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


generateApp :: Can.Expr -> Can.Expr -> State.State Int Core.Expr
generateApp f arg =
  let
    splitFunction apps =
      (head apps, mapM generateExpr (tail apps ++ [arg]))

    (function, generatedArgs) =
      splitFunction (Can.collectApps f)
  in
    case Annotation.drop function of
      Can.Var (Var.Canonical (Var.Module moduleName) name)
        | ModuleName.canonicalIsNative moduleName ->
        Core.Call (moduleToText moduleName) name <$> generatedArgs

      _ ->
        do  fun <- generateExpr function
            args <- generatedArgs
            State.foldM App.reduce2 fun args


generateLambda :: Pattern.Canonical -> Core.Expr -> Core.Expr
generateLambda pattern body =
  let
    immediateCase name match =
      Core.Fun [name] $
        Core.Case (Core.Lit (Core.Var name)) [generateClause match body]
  in
    case Annotation.drop pattern of
      Pattern.Ctor _var _args ->
        immediateCase "_tmp" pattern

      Pattern.Alias name aliased ->
        immediateCase name aliased

      Pattern.Var name ->
        Core.Fun [name] body


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
generateCtor toContext var args =
  if Var.isPrim "[]" var then
    toContext Core.Nil

  else if Var.isPrim "::" var then
    foldl1 (generateCons toContext) args

  else if Var.isTuple var then
    toContext . Core.Tuple $ args

  else
    toContext . Core.Tuple $ toContext (Core.Atom (Var._name var)) : args


generateCons :: (Core.Literal a -> a) -> a -> a -> a
generateCons toContext first =
  toContext . Core.Cons first
