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
      do  body' <- generateExpr body
          return $ gen name (Core.Fun args body')


generateExpr :: Opt.Expr -> State.State Int Core.Expr
generateExpr expr =
  case expr of
    Opt.Literal lit ->
      return $ Core.C (Const.literal lit)

    Opt.Var var ->
      return $ generateVar var

    Opt.List exprs ->
      Subst.many (Core.C . foldr Core.Cons Core.Nil) =<< mapM generateExpr exprs

    Opt.Binop var lhs rhs ->
      generateCall (Opt.Var var) [lhs, rhs]

    Opt.Function args body ->
      do  body' <- generateExpr body
          return $ foldr (\a -> Core.Fun [a]) body' args

    Opt.Call function args ->
      generateCall function args

    Opt.TailCall name _ args ->
      Subst.many (Core.Apply False name) =<< mapM generateExpr args

    Opt.Let defs body ->
      foldr
        (\def state -> generateDef Core.Let def <*> state)
        (generateExpr body)
        defs

    Opt.Case switch decider branches ->
      Core.Case (Core.Var switch)
        <$> generateDecider decider (Map.fromList branches)

    Opt.Ctor var exprs ->
      Subst.many (Core.C . Pattern.ctor var) =<< mapM generateExpr exprs

    Opt.Program _ expr ->
      generateExpr expr

    _ ->
      error "TODO"


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

      Var.BuiltIn ->
        error "TODO: remove. this doesn't exist in upstream/dev"


generateCall :: Opt.Expr -> [Opt.Expr] -> State.State Int Core.Expr
generateCall function args =
  do  args' <-
        mapM generateExpr args

      let applyVar var argument =
            case var of
              Core.Var name -> Core.Apply True name [argument]
              _ -> error "only variable literals can be applied"

      case function of
        Opt.Var (Var.Canonical (Var.Module moduleName) name)
          | ModuleName.canonicalIsNative moduleName ->
          Subst.many (Core.Call (moduleToText moduleName) name) args'

        _ ->
          flip (foldM (Subst.two applyVar)) args' =<< generateExpr function



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
      do  let newMatch =
                foldr Pattern.insert currentMatch testChain

          success' <-
            collectDeciders success branches newMatch

          (success' ++) <$> collectDeciders failure branches currentMatch

    Opt.FanOut _path _tests _fallback ->
      error "TODO: Opt.FanOut"

  where
    singleton a =
      [(currentMatch, a)]
