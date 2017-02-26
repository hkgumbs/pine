{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt
import qualified Generate.ErlangCore.Builder as Core
import qualified Elm.Package as Pkg
import qualified AST.Literal as Literal
import Elm.Compiler.Module (moduleToText, qualifiedVar)


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    body =
      map generateDef (Module.program info)
  in
    Core.functionsToText body



generateDef :: Opt.Def -> Core.Function
generateDef def =
  let
    functionName maybeHome name =
      maybe id qualifiedVar maybeHome name
  in
    case def of
      Opt.Def (Opt.Facts home) name body ->
          Core.Function (functionName home name) (generateExpr body)


generateExpr :: Opt.Expr -> Core.Expr
generateExpr opt =
  case opt of
    Opt.Literal literal ->
      generateLiteral literal

    Opt.Var var ->
      generateVar var

    Opt.List exprs ->
      Core.List (map generateExpr exprs)

    Opt.Function args body ->
      let
        fun argName coreExpr =
          Core.Fun argName coreExpr
      in
        foldr fun (generateExpr body) args

    Opt.Call function args ->
      generateCall function args

    Opt.Ctor name exprs ->
      Core.Tuple (Core.Atom name : map generateExpr exprs)

    Opt.Program _main expr ->
      generateExpr expr


generateLiteral :: Literal.Literal -> Core.Expr
generateLiteral literal =
  case literal of
    Literal.FloatNum n ->
      Core.Float n

    Literal.IntNum n ->
      Core.Int n

    Literal.Chr c ->
      Core.Char c


generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  let
    applyGlobal moduleName =
      Core.Apply (Core.FunctionRef (qualifiedVar moduleName name) 0) []
  in
    case home of
      Var.Local ->
        Core.Var name

      Var.Module moduleName ->
        applyGlobal moduleName

      Var.TopLevel moduleName ->
        applyGlobal moduleName


generateCall :: Opt.Expr -> [Opt.Expr] -> Core.Expr
generateCall function args =
  let
    apply coreExpr elmExpr =
      Core.Apply coreExpr [generateExpr elmExpr]
  in
    case function of
      Opt.Var var@(Var.Canonical (Var.Module modul) name)
        | Var.isNative var ->
          Core.Call (moduleToText modul) name (map generateExpr args)

      _ ->
        foldl apply (generateExpr function) args
