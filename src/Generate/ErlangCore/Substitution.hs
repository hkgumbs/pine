module Generate.ErlangCore.Substitution (app, binop) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text

import qualified Generate.ErlangCore.Builder as Core


app :: Core.Expr -> Core.Expr -> State.State Int Core.Expr
app function arg =
  substitute function $ \f ->
    substitute arg    $ \a ->
      case f of
        Core.Var name ->
          return $ Core.Apply name Nothing [Core.Lit a]

        _ ->
          error "only variables can be applied"


binop :: Text.Text -> Core.Expr -> Core.Expr -> State.State Int Core.Expr
binop name arg1 arg2 =
  substitute arg1   $ \a1 ->
    substitute arg2 $ \a2 ->
      return $ Core.Apply name (Just 2) [Core.Lit a1, Core.Lit a2]


substitute
  :: Core.Expr
  -> (Core.Literal Core.Expr -> State.State Int Core.Expr)
  -> State.State Int Core.Expr
substitute value use =
  case value of
    Core.Lit literal ->
      use literal

    _ ->
      do  name <- fresh
          body <- use (Core.Var name)
          return $ Core.Let name value body


fresh :: State.State Int Text.Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
