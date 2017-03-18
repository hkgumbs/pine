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
import qualified Generate.ErlangCore.Substitution as Subst


generate :: Module.Module (Module.Info [Can.Def]) -> LazyText.Text
generate (Module.Module name _path info) =
  Core.functionsToText $ map (generateDef name) (Module.program info)


generateDef :: ModuleName.Canonical -> Can.Def -> Core.Function
generateDef moduleName (Can.Def _region pattern body _maybeType) =
  case Annotation.drop pattern of
    Pattern.Var name | Helpers.isOp name ->
      uncurry (generateOpDef moduleName name) (Can.collectLambdas body)

    Pattern.Var name ->
      Core.Function (qualifiedVar moduleName name) [] $
        State.evalState (generateExpr body) 1


generateOpDef
  :: ModuleName.Canonical
  -> Text
  -> [Pattern.Canonical]
  -> Can.Expr
  -> Core.Function
generateOpDef moduleName name args body =
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
      return $ Core.C (generateLiteral literal)

    Can.Var var ->
      return $ generateVar var

    Can.List exprs ->
      Subst.list =<< mapM generateExpr exprs

    Can.Binop var lhs rhs ->
      do  left <- generateExpr lhs
          right <- generateExpr rhs
          generateOp var left right

    Can.Lambda pattern body ->
      generateLambda pattern =<< generateExpr body

    Can.App f arg ->
      generateApp f arg

    Can.Ctor var exprs ->
      Subst.ctor (generateCtor var) =<< mapM generateExpr exprs

    Can.Case expr clauses ->
      do  switch <- generateExpr expr
          Core.Case switch <$>
            mapM (\(pat, body) -> generateClause pat <$> generateExpr body) clauses

    Can.Program _main expr ->
      generateExpr expr


generateOp
  :: Var.Canonical
  -> Core.Expr
  -> Core.Expr
  -> State.State Int Core.Expr
generateOp (Var.Canonical home name) =
  let
    moduleName =
      case home of
        Var.Local -> error "infix operators should only be defined in top-level declarations"
        Var.BuiltIn -> error "there should be no built-in infix operators"
        Var.Module moduleName -> moduleName
        Var.TopLevel moduleName -> moduleName
  in
    Subst.binop (qualifiedVar moduleName name)


generateLiteral :: Literal.Literal -> Core.Constant
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
      Core.Apply False (qualifiedVar moduleName name) []
  in
    case home of
      Var.Local ->
        Core.C (Core.Var name)

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
        do  args <- generatedArgs
            Subst.call (moduleToText moduleName) name args

      _ ->
        do  fun <- generateExpr function
            args <- generatedArgs
            State.foldM Subst.apply fun args


generateLambda :: Pattern.Canonical -> Core.Expr -> State.State Int Core.Expr
generateLambda pattern body =
  let
    immediateCase name match =
      Core.Fun [name] $
        Core.Case (Core.C (Core.Var name)) [generateClause match body]
  in
    case Annotation.drop pattern of
      Pattern.Ctor _var _args ->
        do  name <- Subst.fresh
            return $ immediateCase name pattern

      Pattern.Alias name aliased ->
        return $ immediateCase name aliased

      Pattern.Var name ->
        return $ Core.Fun [name] body


generateClause :: Pattern.Canonical -> Core.Expr -> Core.Clause
generateClause pattern expr =
  Core.Clause (generatePattern pattern) (Core.C (Core.Atom "true")) expr


generatePattern :: Pattern.Canonical -> Core.Constant
generatePattern pattern =
  case Annotation.drop pattern of
    Pattern.Anything ->
      Core.Anything

    Pattern.Var name ->
      Core.Var name

    Pattern.Literal literal ->
      generateLiteral literal

    Pattern.Ctor var args ->
      generateCtor var (map generatePattern args)


generateCtor :: Var.Canonical -> [Core.Constant] -> Core.Constant
generateCtor var args =
  if Var.isPrim "[]" var then
    Core.Nil

  else if Var.isPrim "::" var then
    foldl1 Core.Cons args

  else if Var.isTuple var then
    Core.Tuple args

  else
    Core.Tuple $ Core.Atom (Var._name var) : args
