module Generate.ErlangCore.Application (generate, generate1) where


import qualified Generate.ErlangCore.Builder as Core


generate :: Core.Expr -> [Core.Expr] -> Core.Expr
generate function args =
  Core.Apply function args


generate1 :: Core.Expr -> Core.Expr ->  Core.Expr
generate1 function arg =
  Core.Apply function [arg]
