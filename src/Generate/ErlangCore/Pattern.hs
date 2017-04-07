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
        toCorePattern pattern

      let noOpGuard =
            Core.C (Core.Literal (Core.Atom "true"))

      return $ Core.Clause pattern' noOpGuard body


toCorePattern :: Pattern.Canonical -> State.State Int Core.Pattern
toCorePattern (A _ pattern) =
  case pattern of
    Pattern.Ctor (Var.Canonical _ name) args ->
      do  args' <-
            mapM toCorePattern args

          lift . Core.Tuple . (:) (Core.Pattern (Core.Atom name)) $ args'

    Pattern.Record _fields ->
      error
        "TODO: Pattern.Record"

    Pattern.Alias _name _aliased ->
      error
        "TODO: Pattern.Alias"

    Pattern.Var name ->
      lift (Core.Var name)

    Pattern.Anything ->
      lift =<< Core.Var <$> Subst.fresh

    Pattern.Literal lit ->
      lift (Const.literal lit)

  where
    lift =
      return . Core.Pattern



-- CONSTRUCTORS
-- Here because it is intrinsically related to desctructuring
-- Must use the same data structures!


ctor :: Text -> [Core.Expr] -> State.State Int Core.Expr
ctor name =
  Subst.many $
    Core.C . Core.Literal . Core.Tuple . (:) (Core.Literal (Core.Atom name))


ctorAccess :: Int -> Core.Expr -> State.State Int Core.Expr
ctorAccess index =
  Subst.one $ \tup ->
    Core.Call "erlang" "element" [Core.Literal (Core.Int (index + 2)), tup]


list :: [Core.Expr] -> State.State Int Core.Expr
list =
  error
    "TODO: Lists"
