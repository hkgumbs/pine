module Generate.CoreErlang.Substitution
  ( one, many, many1
  ) where

import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.Environment as Env


-- Turn expressions into variable literals using `let`


substitute
  :: Core.Expr
  -> (Core.Literal -> a)
  -> Env.Gen (Core.Expr -> Core.Expr, a)
substitute value use =
  case value of
    Core.Lit literal ->
      return (id, use literal)

    _ ->
      do  name <- Env.freshName
          return (Core.Let name value, use (Core.LTerm (Core.Var name)))



-- PUBLIC


one :: (Core.Literal -> Core.Expr) -> Core.Expr -> Env.Gen Core.Expr
one use expr =
  do  (newUse, expr) <-
        substitute expr use

      return $ newUse expr


many
  :: ([Core.Literal] -> Core.Expr)
  -> [Core.Expr]
  -> Env.Gen Core.Expr
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


many1
  :: (Core.Literal -> [Core.Literal] -> Core.Expr)
  -> Core.Expr
  -> [Core.Expr]
  -> Env.Gen Core.Expr
many1 use first rest =
  do  (newUse, withFirst) <-
        substitute first use

      newUse <$> many withFirst rest
