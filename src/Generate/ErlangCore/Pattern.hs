{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore.Pattern
  ( match
  , ctor, ctorAccess, list
  ) where

import qualified Control.Monad.State as State
import Data.Text (Text)

import qualified AST.Variable as Var
import qualified AST.Pattern as Pattern
import Reporting.Annotation (Annotated(A))

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Constant as Const
import qualified Generate.ErlangCore.Substitution as Subst


match :: Pattern.Canonical -> Core.Expr -> State.State Int Core.Clause
match pattern body =
  do  pattern' <-
        patternToConstant pattern

      return $ Core.Clause pattern' (Core.C (Core.Atom "true")) body



patternToConstant :: Pattern.Canonical -> State.State Int Core.Constant
patternToConstant (A _ pattern) =
  case pattern of
    Pattern.Ctor (Var.Canonical _ name) args ->
      Core.Tuple . (Core.Atom name :) <$> mapM patternToConstant args

    Pattern.Record _fields ->
      error
        "TODO: Pattern.Record"

    Pattern.Alias _name _aliased ->
      error
        "TODO: Pattern.Alias"

    Pattern.Var name ->
      return (Core.Var name)

    Pattern.Anything ->
      Core.Var <$> Subst.fresh

    Pattern.Literal lit ->
      return (Const.literal lit)



-- CONSTRUCTORS
-- Here because it is intrinsically related to desctructuring
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
