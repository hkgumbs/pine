module Generate.CoreErlang.Substitution
  ( one, two, many
  , fresh
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Generate.CoreErlang.Builder as Core



-- Turn expressions into variable literals using `let`


substitute
  :: Core.Expr
  -> (Core.Literal -> a)
  -> State.State Int (Core.Expr -> Core.Expr, a)
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
  do  (newUse, expr) <-
        substitute expr use

      return $ newUse expr


two
  :: (Core.Literal -> Core.Literal -> Core.Expr)
  -> Core.Expr
  -> Core.Expr
  -> State.State Int Core.Expr
two use first second =
  do  (firstUse, firstLit) <-
        substitute first id

      (secondUse, secondLit) <-
        substitute second id

      return $ firstUse (secondUse (use firstLit secondLit))


many
  :: ([Core.Literal] -> Core.Expr)
  -> [Core.Expr]
  -> State.State Int Core.Expr
many use exprs =
  do  let combine next acc =
            do  (outerUse, oldValue) <-
                  acc

                (innerUse, value) <-
                  substitute next (: oldValue)

                return (innerUse . outerUse, value)

      (finalUse, literals) <-
        foldr combine (return (id, [])) exprs

      return $ finalUse (use literals)



-- VARIABLE HELPERS


fresh :: State.State Int Text
fresh =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
