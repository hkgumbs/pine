{-# LANGUAGE OverloadedStrings #-}
module Generate.ErlangCore (generate) where

import Control.Monad (foldM, liftM2)
import qualified Control.Monad.State as State

import qualified Data.ByteString.Builder as BS
import qualified Data.Text as Text

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt
import Elm.Compiler.Module (moduleToText, qualifiedVar)

import qualified Generate.ErlangCore.Builder as Core
import qualified Generate.ErlangCore.Constant as Const
import qualified Generate.ErlangCore.Substitution as Subst
import qualified Generate.ErlangCore.Pattern as Pattern


generate :: Module.Module (Module.Info [Opt.Def]) -> BS.Builder
generate (Module.Module moduleName _path info) =
  let
    function name =
      Core.Function (qualifiedVar moduleName name) []
  in
    Core.encodeUtf8 $
      map (flip State.evalState 1 . generateDef function) (Module.program info)


generateDef :: (Text.Text -> Core.Expr -> a) -> Opt.Def -> State.State Int a
generateDef gen def =
  case def of
    Opt.Def _ name body ->
      gen name <$> generateExpr body

    Opt.TailDef _ name args body ->
      do  body' <-
            generateExpr body

          let args' =
                map (Core.Literal . Core.Var) args

              appliedLetRec =
                Core.Apply Core.FunctionRef name args'

              letRec =
                  Core.LetRec name args body'
                    (foldr (\a -> Core.Fun [a]) appliedLetRec args)

          return (gen name letRec)


generateExpr :: Opt.Expr -> State.State Int Core.Expr
generateExpr expr =
  case expr of
    Opt.Literal lit ->
      return $ Core.C (Core.Literal (Const.literal lit))

    Opt.Var var ->
      return $ generateVar var

    Opt.List exprs ->
      Pattern.list =<< mapM generateExpr exprs

    Opt.Binop var lhs rhs ->
      generateCall (Opt.Var var) [lhs, rhs]

    Opt.Function args body ->
      do  body' <- generateExpr body
          return $ foldr (\a -> Core.Fun [a]) body' args

    Opt.Call function args ->
      generateCall function args

    Opt.TailCall name _ args ->
      Subst.many (Core.Apply Core.FunctionRef name) =<< mapM generateExpr args

    Opt.If branches finally ->
      let
        toBranch bool expr =
          (Core.Pattern (Core.Atom bool), expr)

        toCase (condition, ifTrue) ifFalse =
          do  checks <-
                sequence
                  [ toBranch "true" <$> generateExpr ifTrue
                  , toBranch "false" <$> ifFalse
                  ]

              Subst.one (flip Core.Case checks) =<< generateExpr condition
      in
        foldr toCase (generateExpr finally) branches

    Opt.Let defs body ->
      foldr
        (\def state -> generateDef Core.Let def <*> state)
        (generateExpr body)
        defs

    Opt.Case switch branches ->
      do  let toCore (pattern, expr) =
                liftM2 (,) (Pattern.match pattern) (generateExpr expr)

          branches' <-
            mapM toCore branches

          Subst.one (flip Core.Case branches') =<< generateExpr switch

    Opt.Ctor name exprs ->
      Pattern.ctor name =<< mapM generateExpr exprs

    Opt.CtorAccess expr index ->
      Pattern.ctorAccess index =<< generateExpr expr

    Opt.Access _record _field ->
      error
        "TODO: Opt.Access to Core.Expr"

    Opt.Update _record _fields ->
      error
        "TODO: Opt.Update to Core.Expr"

    Opt.Record _fields ->
      error
        "TODO: Opt.Record to Core.Expr"

    Opt.Cmd _moduleName ->
      error
        "TODO: Opt.Cmd to Core.Expr"

    Opt.Sub _moduleName ->
      error
        "TODO: Opt.Sub to Core.Expr"

    Opt.OutgoingPort _name _type ->
      error
        "TODO: Opt.OutgoingPort to Core.Expr"

    Opt.IncomingPort _name _type ->
      error
        "TODO: Opt.IncomingPort to Core.Expr"

    Opt.Program _type expr ->
      generateExpr expr

    Opt.GLShader _ _ _ ->
      error
        "TODO: Opt.GLShader to Core.Expr"

    Opt.Crash _moduleName _region _maybeExpr ->
      error
        "TODO: Opt.Crash to Core.Expr"



-- VARIABLES


generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  let
    reference moduleName =
      Core.Apply Core.FunctionRef (qualifiedVar moduleName name) []
  in
    case home of
      Var.Local ->
        Core.C (Core.Literal (Core.Var name))

      Var.Module moduleName ->
        reference moduleName

      Var.TopLevel moduleName ->
        reference moduleName

      Var.BuiltIn ->
        error
          "Will go away when merged with upstream dev."


generateCall :: Opt.Expr -> [Opt.Expr] -> State.State Int Core.Expr
generateCall function args =
  do  args' <-
        mapM generateExpr args

      case function of
        Opt.Var (Var.Canonical (Var.Module moduleName) name)
          | ModuleName.canonicalIsNative moduleName ->
          Subst.many (Core.Call (moduleToText moduleName) name) args'

        _ ->
          flip (foldM (Subst.two applyVar)) args' =<< generateExpr function


applyVar :: Core.Literal -> Core.Literal -> Core.Expr
applyVar var argument =
  case var of
    Core.Literal (Core.Var name) ->
      Core.Apply Core.VarRef name [argument]

    _ ->
      error
        "This is an impossible situation \
        \ - trying to call a number, list or something like that."
