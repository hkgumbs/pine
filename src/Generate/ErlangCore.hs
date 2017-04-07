{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import Control.Monad (foldM)
import qualified Control.Monad.State as State

import qualified Data.ByteString.Builder as BS
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Map ((!))

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt
import Elm.Compiler.Module (moduleToText, qualifiedVar)

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Constant as Const
import qualified Generate.ErlangCore.Substitution as Subst
import qualified Generate.ErlangCore.Pattern as Pattern


generate :: Module.Module (Module.Info [Opt.Def]) -> BS.Builder
generate (Module.Module moduleName _path info) =
  let
    function name =
      Core.Function (qualifiedVar moduleName name) []
  in
    Core.encodeUtf8 $
      map (flip State.evalState 1 . generateDef function) (Module.program info)


generateDef :: (Text.Text -> Core.Expr -> a) -> Opt.Def -> State.State Int a
generateDef gen def =
  case def of
    Opt.Def _ name body ->
      gen name <$> generateExpr body

    Opt.TailDef _ name args body ->
      do  body' <-
            generateExpr body

          let appliedLetRec =
                Core.Apply Core.FunctionRef name (map Core.Var args)

              letRec =
                  Core.LetRec name args body'
                    (foldr (\a -> Core.Fun [a]) appliedLetRec args)

          return (gen name letRec)


generateExpr :: Opt.Expr -> State.State Int Core.Expr
generateExpr expr =
  case expr of
    Opt.Literal lit ->
      return $ Core.C (Const.literal lit)

    Opt.Var var ->
      return $ generateVar var

    Opt.List exprs ->
      Pattern.list =<< mapM generateExpr exprs

    Opt.Binop var lhs rhs ->
      generateCall (Opt.Var var) [lhs, rhs]

    Opt.Function args body ->
      do  body' <- generateExpr body
          return $ foldr (\a -> Core.Fun [a]) body' args

    Opt.Call function args ->
      generateCall function args

    Opt.TailCall name _ args ->
      Subst.many (Core.Apply Core.FunctionRef name) =<< mapM generateExpr args

    Opt.If _branches _else ->
      error
        "TODO: Opt.If to Core.Expr"

    Opt.Let defs body ->
      foldr
        (\def state -> generateDef Core.Let def <*> state)
        (generateExpr body)
        defs

    Opt.Case switch decider branches ->
      Core.Case (Core.Var switch)
        <$> generateDecider decider (Map.fromList branches)

    Opt.Ctor name exprs ->
      Pattern.ctor name =<< mapM generateExpr exprs

    Opt.CtorAccess expr index ->
      Pattern.ctorAccess index =<< generateExpr expr

    Opt.Access _record _field ->
      error
        "TODO: Opt.Access to Core.Expr"

    Opt.Update _record _fields ->
      error
        "TODO: Opt.Update to Core.Expr"

    Opt.Record _fields ->
      error
        "TODO: Opt.Record to Core.Expr"

    Opt.Cmd _moduleName ->
      error
        "TODO: Opt.Cmd to Core.Expr"

    Opt.Sub _moduleName ->
      error
        "TODO: Opt.Sub to Core.Expr"

    Opt.OutgoingPort _name _type ->
      error
        "TODO: Opt.OutgoingPort to Core.Expr"

    Opt.IncomingPort _name _type ->
      error
        "TODO: Opt.IncomingPort to Core.Expr"

    Opt.Program _type expr ->
      generateExpr expr

    Opt.GLShader _ _ _ ->
      error
        "TODO: Opt.GLShader to Core.Expr"

    Opt.Crash _moduleName _region _maybeExpr ->
      error
        "TODO: Opt.Crash to Core.Expr"



-- VARIABLES


generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  let
    reference moduleName =
      Core.Apply Core.FunctionRef (qualifiedVar moduleName name) []
  in
    case home of
      Var.Local ->
        Core.C (Core.Var name)

      Var.Module moduleName ->
        reference moduleName

      Var.TopLevel moduleName ->
        reference moduleName

      Var.BuiltIn ->
        error
          "Will go away when merged with upstream dev."


generateCall :: Opt.Expr -> [Opt.Expr] -> State.State Int Core.Expr
generateCall function args =
  do  args' <-
        mapM generateExpr args

      case function of
        Opt.Var (Var.Canonical (Var.Module moduleName) name)
          | ModuleName.canonicalIsNative moduleName ->
          Subst.many (Core.Call (moduleToText moduleName) name) args'

        _ ->
          flip (foldM (Subst.two applyVar)) args' =<< generateExpr function


applyVar :: Core.Constant -> Core.Constant -> Core.Expr
applyVar var argument =
  case var of
    Core.Var name ->
      Core.Apply Core.VarRef name [argument]

    _ ->
      error
        "This is an impossible apply \
        \ - trying to call a tuple, list, or literal."



-- CASE


generateDecider
  :: Opt.Decider Opt.Choice
  -> Map.Map Int Opt.Expr
  -> State.State Int [Core.Clause]
generateDecider decider branches =
  mapM Pattern.toClause =<< collectDeciders decider branches Pattern.new


collectDeciders
  :: Opt.Decider Opt.Choice
  -> Map.Map Int Opt.Expr
  -> Pattern.Match
  -> State.State Int [(Pattern.Match, Core.Expr)]
collectDeciders decider branches currentMatch =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      singleton <$> generateExpr expr

    Opt.Leaf (Opt.Jump i) ->
      singleton <$> generateExpr (branches ! i)

    Opt.Chain testChain success failure ->
      do  let ifMatches =
                Pattern.chain currentMatch testChain

          success' <-
            collectDeciders success branches ifMatches

          failure' <-
            collectDeciders failure branches currentMatch

          return (success' ++ failure')

    Opt.FanOut _path _tests _fallback ->
      error
        "TODO: Opt.FanOut"

  where
    singleton a =
      [(currentMatch, a)]
