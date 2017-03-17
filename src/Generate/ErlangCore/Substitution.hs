module Generate.ErlangCore.Substitution
  ( apply, binop, call
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Generate.ErlangCore.Builder as Core


apply :: Core.Expr -> Core.Expr -> State.State Int Core.Expr
apply function arg =
  substitute function $ \f ->
    substitute arg    $ \a ->
      case f of
        Core.Var name ->
          return $ Core.Apply name Nothing [a]

        _ ->
          error "only variables can be applied"


binop :: Text -> Core.Expr -> Core.Expr -> State.State Int Core.Expr
binop name arg1 arg2 =
  substitute arg1   $ \a1 ->
    substitute arg2 $ \a2 ->
      return $ Core.Apply name (Just 2) [a1, a2]


call :: Text -> Text -> [Core.Expr] -> State.State Int Core.Expr
call modul name =
  let
    addArg expr newArg =
      case expr of
        Core.Call m n args ->
          substitute newArg $ \a -> return $ Core.Call m n (a : args)

        _ ->
          error "something impossible happened"
  in
    State.foldM addArg (Core.Call modul name []) . reverse


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


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
