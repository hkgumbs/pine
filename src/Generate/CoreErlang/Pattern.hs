{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang.Pattern
  ( match
  , ctor, ctorAccess, list
  ) where

import qualified Control.Monad.State as State
import Data.Text (Text)

import qualified AST.Variable as Var
import qualified AST.Pattern as Pattern
import Reporting.Annotation (Annotated(A))

import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.BIF as BIF
import qualified Generate.CoreErlang.Constant as Const
import qualified Generate.CoreErlang.Substitution as Subst


match :: Pattern.Canonical -> State.State Int Core.Pattern
match (A _ pattern) =
  case pattern of
    Pattern.Ctor (Var.Canonical _ "[]") _ ->
      lift Core.Nil

    Pattern.Ctor (Var.Canonical _ "::") [first, rest] ->
      do  first' <-
            match first

          rest' <-
            match rest

          lift (Core.Cons first' rest')

    Pattern.Ctor (Var.Canonical _ name) args ->
      do  args' <-
            mapM match args

          lift (Core.Tuple (Core.Pattern (Core.Atom name) : args'))

    Pattern.Record _fields ->
      error
        "TODO: Pattern.Record"

    Pattern.Alias name aliased ->
      Core.Alias name <$> match aliased

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
  Subst.one (BIF.element (index + 2))


list :: [Core.Expr] -> State.State Int Core.Expr
list =
  Subst.many $ Core.C . foldr
    (\first rest -> Core.Literal (Core.Cons first rest))
    (Core.Literal Core.Nil)
