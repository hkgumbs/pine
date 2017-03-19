{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Control.Monad.State as State
import qualified Data.Text.Lazy as LazyText
import Control.Monad (foldM)
import Data.Text (Text)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Generate.ErlangCore.Builder as Core
import qualified AST.Expression.Canonical as Can
import qualified AST.Literal as Literal
import qualified AST.Expression.Canonical as Can
import qualified AST.Helpers as Helpers
import qualified AST.Pattern
import qualified Reporting.Annotation as Annotation
import Elm.Compiler.Module (moduleToText, qualifiedVar)

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Substitution as Subst
import qualified Generate.ErlangCore.Pattern as Pattern


generate :: Module.Module (Module.Info [Can.Def]) -> LazyText.Text
generate (Module.Module moduleName _path info) =
  let
    defToFunction def =
      State.evalState (generateDef moduleName def) 1
  in
    Core.functionsToText $ map defToFunction (Module.program info)


generateDef :: ModuleName.Canonical -> Can.Def -> State.State Int Core.Function
generateDef moduleName (Can.Def _region pattern body _maybeType) =
  let
    function name =
      Core.Function (qualifiedVar moduleName name)
  in
    case Annotation.drop pattern of
      AST.Pattern.Var name | Helpers.isOp name ->
        generateOpDef (function name) body
      _ ->
        do  b <- generateExpr body
            Pattern.match (\name -> function name []) pattern b


generateOpDef :: ([Text] -> Core.Expr -> a) -> Can.Expr -> State.State Int a
generateOpDef gen body =
  let
    (patterns, function) =
      Can.collectLambdas body

    collectArgs (args, expr) pat =
      do  (name, e) <- Pattern.match (,) pat expr
          return (name : args, e)
  in
    uncurry gen <$>
      do  f <- generateExpr function
          foldM collectArgs ([], f) (reverse patterns)


generateExpr :: Can.Expr -> State.State Int Core.Expr
generateExpr expr =
  case Annotation.drop expr of
    Can.Literal literal ->
      return $ Core.C (Pattern.constant literal)

    Can.Var var ->
      return $ generateVar var

    Can.List exprs ->
      Subst.list =<< mapM generateExpr exprs

    Can.Binop var lhs rhs ->
      do  left <- generateExpr lhs
          right <- generateExpr rhs
          generateOp var left right

    Can.Lambda pattern body ->
      Pattern.match (\arg -> Core.Fun [arg]) pattern =<< generateExpr body

    Can.App f arg ->
      generateApp f arg

    Can.Let defs expr ->
      let
        collectLet e (Can.Def _ pat body _) =
          flip Core.Case [Pattern.clause pat e] <$> generateExpr body
      in
        do  body <- generateExpr expr
            foldM collectLet body (reverse defs)

    Can.Case expr clauses ->
      do  switch <- generateExpr expr
          Core.Case switch <$>
            mapM (\(pat, body) -> Pattern.clause pat <$> generateExpr body) clauses

    Can.Ctor var exprs ->
      Subst.ctor (Pattern.ctor var) =<< mapM generateExpr exprs

    Can.Program _main expr ->
      generateExpr expr


generateOp
  :: Var.Canonical
  -> Core.Expr
  -> Core.Expr
  -> State.State Int Core.Expr
generateOp (Var.Canonical home name) lhs rhs =
  let
    moduleName =
      case home of
        Var.Local -> error "infix operators should only be defined in top-level declarations"
        Var.BuiltIn -> error "there should be no built-in infix operators"
        Var.Module moduleName -> moduleName
        Var.TopLevel moduleName -> moduleName
  in
    Subst.apply False (qualifiedVar moduleName name) [lhs, rhs]


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
            foldM (\f a -> Subst.applyExpr f [a]) fun args
