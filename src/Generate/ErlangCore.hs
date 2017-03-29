{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Monad.State (State, foldM, evalState)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Expression.Optimized as Opt
import qualified Generate.ErlangCore.Builder as Core
import qualified Elm.Package as Pkg
import qualified AST.Literal as Literal



generate :: Module.Optimized -> LazyText.Text
generate (Module.Module moduleName _ info) =
  let
    genBody =
      do  defsList <- mapM generateDef (Module.program info)
          return (concat defsList)

    body =
      evalState genBody 0
  in
    Core.stmtsToText body



generateDef :: Opt.Def -> State Int [Core.Stmt]
generateDef def =
  case def of
    Opt.Def (Opt.Facts home) name body ->
        return [ defineFunction home name [] (generateExpr body) ]


generateExpr :: Opt.Expr -> Core.Expr
generateExpr opt =
  case opt of
    Opt.Literal literal ->
      generateLiteral literal


generateLiteral :: Literal.Literal -> Core.Expr
generateLiteral literal =
  case literal of
    Literal.FloatNum n ->
      Core.Float n

    Literal.IntNum n ->
      Core.Int n


defineFunction :: Maybe ModuleName.Canonical -> Text -> [Text] -> Core.Expr -> Core.Stmt
defineFunction maybeHome functionName args body =
  let
    name =
      maybe id qualified maybeHome functionName
  in
    Core.FunctionStmt (Core.Id name) (map Core.Id args) body


qualified :: ModuleName.Canonical -> Text -> Text
qualified moduleName name =
  moduleToText moduleName <> "@" <> name



moduleToText :: ModuleName.Canonical -> Text
moduleToText (ModuleName.Canonical (Pkg.Name user project) moduleName) =
  let
    safeUser =
      Text.replace "-" "_" user

    safeProject =
      Text.replace "-" "_" project

    safeModuleName =
      Text.replace "." "_" moduleName
  in
    "_" <> safeUser <> "@" <> safeProject <> "@" <> safeModuleName
