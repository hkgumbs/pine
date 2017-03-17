module Generate.ErlangCore.Substitution
  ( apply, binop, call, list, ctor
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Generate.ErlangCore.Builder as Core


-- COLLECTOR


type Collector a
  = State.State Int (Core.Expr -> Core.Expr, a)


run :: (a -> Core.Expr) -> Collector a -> State.State Int Core.Expr
run toExpr state =
  do  (use, a) <- state
      return (use (toExpr a))


substitute :: Core.Expr -> (Core.Literal -> a) -> Collector a
substitute value use =
  case value of
    Core.Lit literal ->
      return (id, use literal)

    _ ->
      do  name <- fresh
          return (Core.Let name value, use (Core.Var name))


foldWith :: (Core.Literal -> a -> a) -> a -> [Core.Expr] -> Collector a
foldWith combine initial =
  let
    fold (outerUse, oldValue) next =
      do  (innerUse, value) <- substitute next (flip combine oldValue)
          return (outerUse . innerUse, value)
  in
    State.foldM fold (id, initial) . reverse




-- GENERATE CORE


apply :: Core.Expr -> Core.Expr -> State.State Int Core.Expr
apply function arg =
  let
    varName f =
      case f of
        Core.Var name ->
          Core.Apply name Nothing

        _ ->
          error "only variable literals can be applied"
  in
    do  (useFun, f) <- substitute function varName
        (useArg, body) <- substitute arg (f . \a -> [a])
        return (useFun (useArg body))


binop :: Text -> Core.Expr -> Core.Expr -> State.State Int Core.Expr
binop name lhs rhs =
  run (Core.Apply name (Just 2)) . foldWith (:) [] $ [lhs, rhs]


call :: Text -> Text -> [Core.Expr] -> State.State Int Core.Expr
call modul name =
  run (Core.Call modul name) . foldWith (:) []


list :: [Core.Expr] -> State.State Int Core.Expr
list =
  run Core.Lit . foldWith Core.Cons Core.Nil


ctor
  :: ([Core.Literal] -> Core.Literal)
  -> [Core.Expr]
  -> State.State Int Core.Expr
ctor toCtor =
  run (Core.Lit . toCtor) . foldWith (:) []



-- VARIABLE HELPERS


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
