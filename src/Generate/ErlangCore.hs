{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))

import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified AST.Module.Name as ModuleName
import qualified AST.Expression.Optimized as Opt
import qualified Generate.ErlangCore.Builder as Core
import qualified Elm.Package as Pkg
import qualified AST.Literal as Literal


generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    body =
      map generateDef (Module.program info)
  in
    Core.stmtsToText body



generateDef :: Opt.Def -> Core.Stmt
generateDef def =
  case def of
    Opt.Def (Opt.Facts home) name body ->
        defineFunction home name (generateExpr body)


generateExpr :: Opt.Expr -> Core.Expr
generateExpr opt =
  case opt of
    Opt.Literal literal ->
      generateLiteral literal

    Opt.List exprs ->
      Core.List (map generateExpr exprs)

    Opt.Var var ->
      generateVar var

    Opt.Function args body ->
      let
        generateFunction [] =
          generateExpr body

        generateFunction (first : rest) =
          Core.Function (Core.Id first) (generateFunction rest)
      in
        generateFunction args

generateLiteral :: Literal.Literal -> Core.Expr
generateLiteral literal =
  case literal of
    Literal.FloatNum n ->
      Core.Float n

    Literal.IntNum n ->
      Core.Int n


defineFunction :: Maybe ModuleName.Canonical -> Text -> Core.Expr -> Core.Stmt
defineFunction maybeHome functionName body =
  let
    name =
      maybe id qualified maybeHome functionName
  in
    Core.FunctionStmt (Core.Id name) body

generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  case home of
    Var.Local ->
      Core.Var (Core.Id name)

    Var.Module moduleName ->
      Core.Apply (Core.Id (qualified moduleName name))

    Var.TopLevel moduleName ->
      Core.Apply (Core.Id (qualified moduleName name))


qualified :: ModuleName.Canonical -> Text -> Text
qualified (ModuleName.Canonical (Pkg.Name user project) moduleName) name =
  let
    safeUser =
      Text.replace "-" "_" user

    safeProject =
      Text.replace "-" "_" project

    safeModuleName =
      Text.replace "." "_" moduleName
  in
    safeUser <> "@" <> safeProject <> "@" <> safeModuleName <> "@" <> name
