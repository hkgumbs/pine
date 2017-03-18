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
      use name $
        Core.Case (Core.C (Core.Var name)) [clause match expr]
  in
    case Annotation.drop pattern of
      Pattern.Ctor _var _args ->
        do  name <- Subst.fresh
            return $ deconstruct name pattern

      Pattern.Alias name aliased ->
        return $ deconstruct name aliased

      Pattern.Var name ->
        return $ use name expr


clause :: Pattern.Canonical -> Core.Expr -> Core.Clause
clause pattern expr =
  Core.Clause (toConstant pattern) (Core.C (Core.Atom "true")) expr


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


toConstant :: Pattern.Canonical -> Core.Constant
toConstant pattern =
  case Annotation.drop pattern of
    Pattern.Anything ->
      Core.Anything

    Pattern.Var name ->
      Core.Var name

    Pattern.Literal literal ->
      constant literal

    Pattern.Ctor var args ->
      ctor var (map toConstant args)
