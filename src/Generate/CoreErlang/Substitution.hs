module Generate.CoreErlang.Substitution
  ( one, two, many
  , fresh
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Generate.CoreErlang.Builder as Core



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


substitute :: Core.Expr -> (Core.Literal -> a) -> Collector a
substitute value use =
  case value of
    Core.Lit literal ->
      return (id, use literal)

    _ ->
      do  name <- fresh
          return (Core.Let name value, use (Core.LTerm (Core.Var name)))



-- PUBLIC


one :: (Core.Literal -> Core.Expr) -> Core.Expr -> State.State Int Core.Expr
one use expr =
  substitute expr use |> id


two
  :: (Core.Literal -> Core.Literal -> Core.Expr)
  -> Core.Expr
  -> Core.Expr
  -> State.State Int Core.Expr
two use first second =
  do  (firstUse, firstC) <-
        substitute first id

      (secondUse, secondC) <-
        substitute second id

      return $ firstUse (secondUse (use firstC secondC))


many
  :: ([Core.Literal] -> Core.Expr)
  -> [Core.Expr]
  -> State.State Int Core.Expr
many use exprs =
  let
    combine next acc =
      do  (outerUse, oldValue) <-
            acc

          (innerUse, value) <-
            substitute next (: oldValue)

          return (innerUse . outerUse, value)
  in
    foldr combine (return (id, [])) exprs |> use



-- VARIABLE HELPERS


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
