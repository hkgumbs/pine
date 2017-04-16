{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang.Pattern
  ( match
  , ctor, ctorAccess, list
  ) where

import Data.Text (Text)

import qualified AST.Variable as Var
import qualified AST.Pattern as Pattern
import Reporting.Annotation (Annotated(A))

import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.BuiltIn as BuiltIn
import qualified Generate.CoreErlang.Environment as Env
import qualified Generate.CoreErlang.Literal as Literal
import qualified Generate.CoreErlang.Substitution as Subst


match :: Pattern.Canonical -> Env.Gen Core.Pattern
match (A _ pattern) =
  case pattern of
    Pattern.Ctor (Var.Canonical _ "[]") _ ->
      return $ Core.PTerm Core.Nil

    Pattern.Ctor (Var.Canonical _ "::") [first, rest] ->
      do  first' <-
            match first

          rest' <-
            match rest

          return (Core.PCons first' rest')

    Pattern.Ctor (Var.Canonical _ name) args ->
      do  args' <-
            mapM match args

          return $ Core.PTuple (Core.PTerm (Core.Atom name) : args')

    Pattern.Record fields ->
      let
        toPair name =
          (Core.PTerm (Core.Atom name), Core.PTerm (Core.Var name))
      in
        return $ Core.PMap (map toPair fields)

    Pattern.Alias name aliased ->
      Core.PAlias name <$> match aliased

    Pattern.Var name ->
      return $ Core.PTerm (Core.Var name)

    Pattern.Anything ->
      Core.PTerm <$> Core.Var <$> Env.freshName

    Pattern.Literal lit ->
      return $ Core.PTerm (Literal.term lit)



-- CONSTRUCTORS
-- Here because it is intrinsically related to desctructuring
-- Must use the same data structures!


ctor :: Text -> [Core.Expr] -> Env.Gen Core.Expr
ctor name =
  Subst.many $
    Core.Lit . Core.LTuple . (:) (Core.LTerm (Core.Atom name))


ctorAccess :: Int -> Core.Expr -> Env.Gen Core.Expr
ctorAccess index =
  Subst.one (BuiltIn.element (index + 2))


list :: [Core.Expr] -> Env.Gen Core.Expr
list =
  Subst.many $ Core.Lit . foldr
    (\first rest -> Core.LCons first rest)
    (Core.LTerm Core.Nil)
