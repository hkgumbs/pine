{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt
import qualified Generate.ErlangCore.Builder as Core
import qualified AST.Literal as Literal

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Function as Function
import qualified Generate.ErlangCore.String as String


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    body =
      map generateDef (Module.program info)
  in
    Core.functionsToText body



generateDef :: Opt.Def -> Core.Function
generateDef def =
  case def of
    Opt.Def (Opt.Facts (Just home)) name body ->
        Function.makeTopLevel home name (generateExpr body)


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
          Core.Fun [argName] coreExpr
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


generateCall :: Opt.Expr -> [Opt.Expr] -> Core.Expr
generateCall function args =
  let
    apply coreExpr elmExpr =
      Core.Apply coreExpr [generateExpr elmExpr]
  in
    case function of
      Opt.Var var@(Var.Canonical (Var.Module modul) name)
        | Var.isNative var ->
          Function.externalCall modul name (map generateExpr args)

      _ ->
        foldl apply (generateExpr function) args
