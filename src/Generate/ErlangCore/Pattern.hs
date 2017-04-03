{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Pattern
  ( Match, new, insert
  , toClause
  ) where

import qualified Control.Monad.State as State
import Control.Monad (foldM)

import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Constant as Constant
import qualified Generate.ErlangCore.Substitution as Subst


data Match
  = Test DT.Test
  | Placeholder
  | Group [Match]


new :: Match
new =
  Placeholder


insert :: (DT.Path, DT.Test) -> Match -> Match
insert (path, test) =
  combine (toMatch path test)


toMatch :: DT.Path -> DT.Test -> Match
toMatch path test =
  case path of
    DT.Position i subPath ->
      Group (replicate i Placeholder ++ [toMatch subPath test])

    DT.Field _text _subPath ->
      error "TODO: DecisionTree.Field to Pattern.Match"

    DT.Empty ->
      Test test

    DT.Alias ->
      Test test


combine :: Match -> Match -> Match
combine first second =
  case (first, second) of
    (_, Placeholder) ->
      first

    (Placeholder, _) ->
      second

    (Group first, Group second) ->
      Group $ take
        (max (length first) (length second))
        (zipWith combine
          (first ++ repeat Placeholder)
          (second ++ repeat Placeholder))

    (_, _) ->
      error "Something tricky happened while pattern-matching!"


toClause :: (Match, Core.Expr) -> State.State Int Core.Clause
toClause (match, body) =
  do  c <-
        toConstant match

      return $ Core.Clause c (Core.C (Core.Atom "true")) body


toConstant :: Match -> State.State Int Core.Constant
toConstant match =
  case match of
    Test (DT.Constructor (Var.Canonical _ name)) ->
      return (Core.Atom name)

    Test (DT.Literal lit) ->
      return (Constant.literal lit)

    Placeholder ->
      Core.Var <$> Subst.fresh

    Group matches ->
      do  tail <-
            Core.Var <$> Subst.fresh

          foldM collectMatch tail (reverse matches)

  where
    collectMatch acc next =
      do  next' <-
            toConstant next

          return $ Core.Cons next' acc
