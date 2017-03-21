{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Pattern
  ( match, clause, ctor, constant
  ) where

import qualified Control.Monad.State as State
import Data.Text (Text)

import qualified AST.Variable as Var
import qualified AST.Literal as Literal
import qualified AST.Pattern as Pattern
import qualified Reporting.Annotation as Annotation

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.String as String
import qualified Generate.ErlangCore.Substitution as Subst


match
  :: (Text -> Core.Expr -> a)
  -> Pattern.Canonical
  -> Core.Expr
  -> State.State Int a
match use pattern expr =
  let
    deconstruct name match =
      do  c <- clause match expr
          return $ use name (Core.Case (Core.Var name) [c])
  in
    case Annotation.drop pattern of
      Pattern.Ctor _var _args ->
        do  name <- Subst.fresh
            deconstruct name pattern

      Pattern.Alias name aliased ->
        deconstruct name aliased

      Pattern.Var name ->
        return $ use name expr


clause :: Pattern.Canonical -> Core.Expr -> State.State Int Core.Clause
clause pattern expr =
  let
    toCore p =
      case Annotation.drop p of
        Pattern.Anything ->
          Core.Var <$> Subst.fresh

        Pattern.Var name ->
          return $ Core.Var name

        Pattern.Literal literal ->
          return $ constant literal

        Pattern.Ctor var args ->
          ctor var <$> mapM toCore args
  in
    do  p <- toCore pattern
        return $ Core.Clause p (Core.C (Core.Atom "true")) expr


ctor :: Var.Canonical -> [Core.Constant] -> Core.Constant
ctor var args =
  if Var.isPrim "[]" var then
    Core.Nil

  else if Var.isPrim "::" var then
    foldl1 Core.Cons args

  else if Var.isTuple var then
    Core.Tuple args

  else
    Core.Tuple $ Core.Atom (Var._name var) : args


constant :: Literal.Literal -> Core.Constant
constant literal =
  case literal of
    Literal.Chr c ->
      String.character c

    Literal.Str text ->
      String.bitString text

    Literal.IntNum n ->
      Core.Int n

    Literal.FloatNum n ->
      Core.Float n

    Literal.Boolean b ->
      Core.Atom $ if b then "true" else "false"
