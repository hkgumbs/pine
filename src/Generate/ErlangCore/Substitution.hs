module Generate.ErlangCore.Substitution (app, app2) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text

import qualified Generate.ErlangCore.Builder as Core


app :: Core.Expr -> Core.Expr -> State.State Int Core.Expr
app function arg =
  substitute function $ \f ->
    substitute arg    $ \a ->
      return $ Core.Apply f [a]


app2 :: Core.Expr -> Core.Expr -> Core.Expr -> State.State Int Core.Expr
app2 function arg1 arg2 =
  substitute function $ \f ->
    substitute arg1   $ \a1 ->
      substitute arg2 $ \a2 ->
        return $ Core.Apply f [a1, a2]


substitute
  :: Core.Expr
  -> (Core.Expr -> State.State Int Core.Expr)
  -> State.State Int Core.Expr
substitute value use =
  case value of
    Core.Lit _ ->
      use value

    Core.FunctionRef _ _ ->
      use value

    _ ->
      do  name <- fresh
          body <- use (Core.Lit (Core.Var name))
          return $ Core.Let name value body


fresh :: State.State Int Text.Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
