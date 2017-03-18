module Generate.ErlangCore.Substitution
  ( apply, binop, call, list, ctor
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


run :: (a -> Core.Expr) -> Collector a -> State.State Int Core.Expr
run toExpr state =
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
    State.foldM fold (id, initial)




-- GENERATE CORE


apply :: Core.Expr -> Core.Expr -> State.State Int Core.Expr
apply function arg =
  let
    varName f =
      case f of
        Core.Var name ->
          Core.Apply True name

        _ ->
          error "only variable literals can be applied"
  in
    do  (useFun, f) <- substitute function varName
        (useArg, body) <- substitute arg (f . \a -> [a])
        return (useFun (useArg body))


binop :: Text -> Core.Expr -> Core.Expr -> State.State Int Core.Expr
binop name lhs rhs =
  run (Core.Apply False name) $ foldWith (:) [] [rhs, lhs]


call :: Text -> Text -> [Core.Expr] -> State.State Int Core.Expr
call modul name =
  run (Core.Call modul name) . foldWith (:) [] . reverse


list :: [Core.Expr] -> State.State Int Core.Expr
list =
  run Core.C . foldWith Core.Cons Core.Nil . reverse


ctor
  :: ([Core.Constant] -> Core.Constant)
  -> [Core.Expr]
  -> State.State Int Core.Expr
ctor toCtor =
  run (Core.C . toCtor) . foldWith (:) [] . reverse



-- VARIABLE HELPERS


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
