{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import Control.Monad (foldM)
import qualified Control.Monad.State as State

import qualified Data.ByteString.Builder as BS
import qualified Data.Text as Text
import Data.Monoid ((<>))

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt
import qualified Optimize.DecisionTree as DT
import Elm.Compiler.Module (moduleToText, qualifiedVar)

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Substitution as Subst
import qualified Generate.ErlangCore.Constant as Constant


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
      return $ Core.C (Constant.literal lit)

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
      generateCase switch decider branches

    Opt.Ctor var exprs ->
      Subst.many (Core.C . Constant.ctor var) =<< mapM generateExpr exprs

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


generateCase
  :: Text.Text
  -> Opt.Decider Opt.Choice
  -> [(Int, Opt.Expr)]
  -> State.State Int Core.Expr
generateCase switch decider branches =
  do  root <-
        Subst.fresh

      let toTarget i =
            root <> "_" <> Text.pack (show i)

          label transform (i, expr) =
            (transform .) <$> Core.Let (toTarget i) <$> generateExpr expr

      withLabels <-
        foldM label id branches

      withLabels
        <$> generateDecider (Core.Var switch) toTarget decider


generateDecider
  :: Core.Constant
  -> (Int -> Text.Text)
  -> Opt.Decider Opt.Choice
  -> State.State Int Core.Expr
generateDecider switch toTarget decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      generateExpr expr

    Opt.Leaf (Opt.Jump i) ->
      return $ Core.C (Core.Var (toTarget i))

    Opt.Chain testChain success failure ->
      do  toClause <-
            Core.Clause <$> Core.Var <$> Subst.fresh

          testChain' <-
            generateTestChain switch testChain

          success' <-
            generateDecider switch toTarget success

          failure' <-
            generateDecider switch toTarget failure

          return $ Core.Case switch
            [ toClause testChain' success'
            , toClause (Core.C (Core.Atom "true")) failure'
            ]

    Opt.FanOut _path _tests _fallback ->
      error "TODO: Opt.FanOut"


generateTestChain
  :: Core.Constant
  -> [(DT.Path, DT.Test)]
  -> State.State Int Core.Expr
generateTestChain switch chain =
  let
    check (path, test) =
      generateTest test =<< generatePath path (Core.C switch)

    combine left right =
      do  left' <- left
          right' <- right
          Subst.many (Core.Call "erlang" "and") [left', right']
  in
    foldl1 combine (map check chain)


generateTest :: DT.Test -> Core.Expr -> State.State Int Core.Expr
generateTest test expr =
  case test of
    DT.Constructor (Var.Canonical _ name) ->
      do  element <-
            Subst.one
              (\arg -> Core.Call "erlang" "element" [Core.Int 1, arg])
              expr

          Subst.one
            (\arg -> Core.Call "erlang" "=:=" [Core.Atom name, arg])
            element

    DT.Literal lit ->
      Subst.one
        (\arg -> Core.Call "erlang" "=:=" [arg , Constant.literal lit])
        expr


generatePath :: DT.Path -> Core.Expr -> State.State Int Core.Expr
generatePath path root =
  case path of
    DT.Position i subPath ->
      do  field <-
            Subst.one
              (\arg -> Core.Call "erlang" "element" [Core.Int (i + 1), arg])
              root

          generatePath subPath field

    DT.Field _text _path ->
      error "TODO: DecisionTree.Field"

    DT.Empty ->
      return root

    DT.Alias ->
      return root
