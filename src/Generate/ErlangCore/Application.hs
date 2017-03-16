module Generate.ErlangCore.Application (reduce2, reduce3) where


import qualified Control.Monad.State as State
import Control.Monad.State (State)

import qualified Data.Text as Text

import qualified Generate.ErlangCore.Builder as Core


reduce2 :: Core.Expr -> Core.Expr -> State.State Int Core.Expr
reduce2 function arg =
  function <@> \f ->
    arg <@> \a ->
      return $ Core.Apply f [a]


reduce3 :: Core.Expr -> Core.Expr -> Core.Expr -> State.State Int Core.Expr
reduce3 function arg1 arg2 =
  function <@> \f ->
    arg1 <@> \a1 ->
      arg2 <@> \a2 ->
        return $ Core.Apply f [a1, a2]


(<@>) :: Core.Expr -> (Core.Expr -> State Int Core.Expr) -> State Int Core.Expr
(<@>) f apply =
  case f of
    Core.Lit _ ->
      apply f

    Core.FunctionRef _ _ ->
      apply f

    _ ->
      do  name <- fresh
          body <- apply (Core.Lit (Core.Var name))
          return $ Core.Let name f body


fresh :: State.State Int Text.Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
