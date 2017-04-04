{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Pattern
  ( Match, new, chain
  , toClause
  , ctor, ctorAccess, list
  ) where

import qualified Control.Monad.State as State
import Data.Text (Text)

import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Constant as Const
import qualified Generate.ErlangCore.Substitution as Subst


data Match
  = Test Core.Constant
  | Placeholder
  | Group [Match]


new :: Match
new =
  Placeholder


chain :: Match -> [(DT.Path, DT.Test)] -> Match
chain =
  foldr $ combine . uncurry toMatch


toMatch :: DT.Path -> DT.Test -> Match
toMatch path test =
  case path of
    DT.Position i size subPath ->
      Group $ concat
        [ replicate (i + 1) Placeholder
        , [ toMatch subPath test ]
        , replicate (size - i - 1) Placeholder
        ]

    DT.Field _text _subPath ->
      error "TODO: DecisionTree.Field to Pattern.Match"

    DT.Empty ->
      testToMatch test

    DT.Alias ->
      testToMatch test


testToMatch :: DT.Test -> Match
testToMatch test =
  case test of
    DT.Constructor (Var.Canonical _ name) size ->
      Group (Test (Core.Atom name) : replicate size Placeholder)

    DT.Literal lit ->
      Test (Const.literal lit)


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
    Test constant ->
      return constant

    Placeholder ->
      Core.Var <$> Subst.fresh

    Group matches ->
      Core.Tuple <$> mapM toConstant matches



-- CONSTRUCTORS
-- Here because it is intrinsically related to the Group notion
-- Must use the same data structures!


ctor :: Text -> [Core.Expr] -> State.State Int Core.Expr
ctor name =
  Subst.many $
    Core.C . Core.Tuple . (++) [Core.Atom name]


ctorAccess :: Int -> Core.Expr -> State.State Int Core.Expr
ctorAccess index =
  Subst.one $ \tup ->
    Core.Call "erlang" "element" [Core.Int (index + 2), tup]


list :: [Core.Expr] -> State.State Int Core.Expr
list =
  Subst.many $ Core.C . foldr
    (\h t -> Core.Tuple [Core.Atom "::", h, t])
    (Core.Tuple [Core.Atom "[]"])
