module Generate.ErlangCore.Substitution
  ( applyExpr, apply, call, list, ctor, case_
  , fresh
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Generate.ErlangCore.Builder as Core



-- COLLECTOR

{-| Keeps track of the variables that we want to bind with `let`.
 -  This lets us think in terms of just the thing we're accumulating,
 -  and bind all of the values afterwards.
 -}

type Collector a
  = State.State Int (Core.Expr -> Core.Expr, a)


(|>) :: Collector a -> (a -> Core.Expr) -> State.State Int Core.Expr
(|>) state toExpr =
  do  (use, a) <- state
      return (use (toExpr a))


substitute :: Core.Expr -> (Core.Constant -> a) -> Collector a
substitute value use =
  case value of
    Core.C constant ->
      return (id, use constant)

    _ ->
      do  name <- fresh
          return (Core.Let name value, use (Core.Var name))


foldWith :: (Core.Constant -> a -> a) -> a -> [Core.Expr] -> Collector a
foldWith combine initial =
  let
    fold (outerUse, oldValue) next =
      do  (innerUse, value) <- substitute next (flip combine oldValue)
          return (innerUse . outerUse, value)
  in
    State.foldM fold (id, initial) . reverse



-- GENERATE CORE


applyExpr :: Core.Expr -> [Core.Expr] -> State.State Int Core.Expr
applyExpr function args =
  let
    varName f =
      case f of
        Core.Var name ->
          name

        _ ->
          error "only variable literals can be applied"
  in
    substitute function varName >>= \(use, name) ->
      use <$> apply True name args


apply :: Bool -> Text -> [Core.Expr] -> State.State Int Core.Expr
apply isVariable name exprs =
  foldWith (:) [] exprs
    |> Core.Apply isVariable name


call :: Text -> Text -> [Core.Expr] -> State.State Int Core.Expr
call modul name exprs =
  foldWith (:) [] exprs
    |> Core.Call modul name


list :: [Core.Expr] -> State.State Int Core.Expr
list exprs =
  foldWith Core.Cons Core.Nil exprs
    |> Core.C


ctor
  :: ([Core.Constant] -> Core.Constant)
  -> [Core.Expr]
  -> State.State Int Core.Expr
ctor toCtor exprs =
  foldWith (:) [] exprs
    |> (Core.C . toCtor)


case_ :: Core.Expr -> [Core.Clause] -> State.State Int Core.Expr
case_ switch clauses =
  do  (use, value) <- substitute switch (\c -> Core.Case c clauses)
      return $ use value



-- VARIABLE HELPERS


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
