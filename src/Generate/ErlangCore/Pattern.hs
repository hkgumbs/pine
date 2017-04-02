{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Pattern (Match, insert, toClause) where

import qualified Control.Monad.State as State

import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Constant as Constant
import qualified Generate.ErlangCore.Substitution as Subst


data Match
  = Check Core.Constant
  | Placeholder
  | Deeper [Match]


insert :: (DT.Path, DT.Test) -> [Match] -> [Match]
insert (path, test) matches =
  case path of
    DT.Position i subPath ->
      if i > length matches then
        matches
        ++ replicate (i - length matches - 1) Placeholder
        ++ [Deeper (insert (subPath, test) [])]

      else
        take (i - 1) matches
        ++ [Deeper (insert (subPath, test) [])]
        ++ drop i matches

    DT.Field _text _subPath ->
      error "TODO: DecisionTree.Field to Pattern.Match"

    DT.Empty ->
      [fromTest test]

    DT.Alias ->
      [fromTest test]

  where
    fromTest (DT.Constructor (Var.Canonical _ name)) =
      Check (Core.Atom name)

    fromTest (DT.Literal lit) =
      Check (Constant.literal lit)


toClause :: ([Match], Core.Expr) -> State.State Int Core.Clause
toClause (matches, body) =
  do  c <-
        toConstant (Deeper matches)

      return $ Core.Clause c (Core.C (Core.Atom "true")) body


toConstant :: Match -> State.State Int Core.Constant
toConstant match =
  case match of
    Check constant ->
      return constant

    Placeholder ->
      Core.Var <$> Subst.fresh

    Deeper matches ->
      do  tail <-
            Core.Var <$> Subst.fresh

          State.foldM collectMatch tail (reverse matches)

  where
    collectMatch acc next =
      do  next' <-
            toConstant next

          return $ Core.Cons next' acc
