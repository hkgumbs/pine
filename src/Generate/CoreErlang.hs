{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang (generate) where

import Control.Monad (liftM2)
import qualified Control.Monad.State as State

import qualified Data.ByteString.Builder as BS
import Data.Text (Text)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt

import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.BuiltIn as BuiltIn
import qualified Generate.CoreErlang.Function as Function
import qualified Generate.CoreErlang.Literal as Literal
import qualified Generate.CoreErlang.Substitution as Subst
import qualified Generate.CoreErlang.Pattern as Pattern


generate :: Module.Module (Module.Info [Opt.Def]) -> BS.Builder
generate (Module.Module moduleName _path info) =
  Core.encodeUtf8 $
    map
      (flip State.evalState 1 . generateDef (Function.topLevel moduleName))
      (Module.program info)


generateDef
  :: (Text -> [Text] -> Core.Expr -> a)
  -> Opt.Def
  -> State.State Int a
generateDef gen def =
  case def of
    Opt.Def _ name (Opt.Function args body) ->
      gen name args <$> generateExpr body

    Opt.Def _ name body ->
      gen name [] <$> generateExpr body

    Opt.TailDef _ name args body ->
      do  body' <-
            generateExpr body

          let letRec =
                Core.LetRec name args body'
                  $ Core.Apply (Core.LFunction name (length args))
                  $ map (Core.LTerm . Core.Var) args

          return (gen name args letRec)


generateExpr :: Opt.Expr -> State.State Int Core.Expr
generateExpr opt =
  case opt of
    Opt.Literal lit ->
      return $ Core.Lit (Core.LTerm (Literal.term lit))

    Opt.Var var ->
      return $ generateVar var

    Opt.List exprs ->
      Pattern.list =<< mapM generateExpr exprs

    Opt.Binop var lhs rhs ->
      Function.binop var =<< mapM generateExpr [lhs, rhs]

    Opt.Function args body ->
      Function.anonymous args <$> generateExpr body

    Opt.Call function args ->
      generateCall function =<< mapM generateExpr args

    Opt.TailCall name _ args ->
      let
        function =
          Core.LFunction name (length args)
      in
        Subst.many (Core.Apply function) =<< mapM generateExpr args

    Opt.If branches finally ->
      let
        toBranch bool expr =
          (Core.PTerm (Core.Atom bool), expr)

        toCase (condition, ifTrue) ifFalse =
          do  checks <-
                sequence
                  [ toBranch "true" <$> generateExpr ifTrue
                  , toBranch "false" <$> ifFalse
                  ]

              Subst.one (flip Core.Case checks) =<< generateExpr condition
      in
        foldr toCase (generateExpr finally) branches

    Opt.Let defs expr ->
      let
        toLet name args body =
          Core.Let name (Function.anonymous args body)
      in
        foldr
          (\def state -> generateDef toLet def <*> state)
          (generateExpr expr)
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

    Opt.Access record field ->
      Subst.one (BuiltIn.get field) =<< generateExpr record

    Opt.Update record fields ->
      let
        zipper literals =
          Core.Update
            (zip (generateKeys fields) (tail literals))
            (head literals)
      in
        Subst.many zipper =<< mapM generateExpr (record : map snd fields)

    Opt.Record fields ->
      do  values <-
            mapM (generateExpr . snd) fields

          Subst.many (Core.Map . zip (generateKeys fields)) values

    Opt.Cmd moduleName ->
      return $ BuiltIn.effect moduleName

    Opt.Sub moduleName ->
      return $ BuiltIn.effect moduleName

    Opt.OutgoingPort _name _type ->
      error
        "TODO: Opt.OutgoingPort to Core.Expr"

    Opt.IncomingPort _name _type ->
      error
        "TODO: Opt.IncomingPort to Core.Expr"

    Opt.Program _type expr ->
      -- TODO: use the type to decode argument
      generateExpr expr

    Opt.GLShader _ _ _ ->
      -- TODO: should likely remove this from the AST
      error
        "Shaders can't be used with the BEAM compiler!"

    Opt.Crash _moduleName _region _maybeExpr ->
      error
        "TODO: Opt.Crash to Core.Expr"



--- VARIABLES


generateVar :: Var.Canonical -> Core.Expr
generateVar (Var.Canonical home name) =
  case home of
    Var.Local ->
      Core.Lit (Core.LTerm (Core.Var name))

    Var.Module moduleName ->
      Function.reference moduleName name

    Var.TopLevel moduleName ->
      Function.reference moduleName name

    Var.BuiltIn ->
      error
        "Will go away when merged with upstream dev."


generateCall :: Opt.Expr -> [Core.Expr] -> State.State Int Core.Expr
generateCall function args =
  case function of
    Opt.Var (Var.Canonical (Var.Module moduleName) name)
      | ModuleName.canonicalIsNative moduleName ->
      Function.nativeCall moduleName name args

    _ ->
      do  function' <-
            generateExpr function

          Function.apply function' args



-- RECORDS


generateKeys :: [(Text, a)] -> [Core.Literal]
generateKeys =
  map (Core.LTerm . Core.Atom . fst)
