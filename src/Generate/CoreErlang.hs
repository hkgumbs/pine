{-# LANGUAGE OverloadedStrings #-}
module Generate.CoreErlang (generate) where

import qualified Data.ByteString.Builder as BS
import qualified Data.Text as Text
import Data.Text (Text)

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified AST.Expression.Optimized as Opt
import Elm.Compiler.Module (qualifiedVar)

import qualified Generate.CoreErlang.Builder as Core
import qualified Generate.CoreErlang.BuiltIn as BuiltIn
import qualified Generate.CoreErlang.Environment as Env
import qualified Generate.CoreErlang.Literal as Literal
import qualified Generate.CoreErlang.Substitution as Subst
import qualified Generate.CoreErlang.Pattern as Pattern


generate
  :: Module.Interfaces
  -> Module.Module (Module.Info [Opt.Def])
  -> BS.Builder
generate interfaces (Module.Module moduleName _ info) =
  let
    topLevel =
      genFunction . qualifiedVar moduleName
  in
    Core.encodeUtf8 $ map
      (Env.run moduleName interfaces . genDef topLevel)
      (Module.program info)


genDef :: (Text -> [Text] -> Opt.Expr -> a) -> Opt.Def -> a
genDef gen def =
    case def of
      Opt.Def _ name (Opt.Function args body) ->
        gen name args body

      Opt.Def _ name body ->
        gen name [] body

      Opt.TailDef _ name args body ->
        gen name args body


genExpr :: Opt.Expr -> Env.Gen Core.Expr
genExpr opt =
  case opt of
    Opt.Literal lit ->
      return $ Core.Lit (Core.LTerm (Literal.term lit))

    Opt.Var var ->
      genVar var

    Opt.List exprs ->
      Pattern.list =<< mapM genExpr exprs

    Opt.Binop var lhs rhs ->
      genBinop var =<< mapM genExpr [lhs, rhs]

    Opt.Function args body ->
      Env.withLocals (genLocals args) $
        Core.Fun args <$> genExpr body

    Opt.Call function args ->
      genCall function =<< mapM genExpr args

    Opt.TailCall name _ args ->
      do  qualified <-
            maybe name (flip qualifiedVar name) <$> Env.findNearest name

          let apply =
                Core.Apply (Core.LFunction qualified (length args))

          Subst.many apply =<< mapM genExpr args

    Opt.If branches finally ->
      let
        toBranch bool expr =
          (Core.PTerm (Core.Atom bool), expr)

        toCase (condition, ifTrue) ifFalse =
          do  checks <-
                sequence
                  [ toBranch "true" <$> genExpr ifTrue
                  , toBranch "false" <$> ifFalse
                  ]

              Subst.one (flip Core.Case checks) =<< genExpr condition
      in
        foldr toCase (genExpr finally) branches

    Opt.Let defs expr ->
      genLet defs expr

    Opt.Case switch branches ->
      let
        toCore (pattern, expr) =
          do  pattern' <-
                Pattern.match pattern

              expr' <-
                genExpr expr

              return (pattern', expr')

        toLocals =
          genLocals . concatMap (Pattern.names . fst)
      in
        Env.withLocals (toLocals branches) $
          do  branches' <-
                mapM toCore branches

              Subst.one (flip Core.Case branches') =<< genExpr switch

    Opt.Ctor name exprs ->
      Pattern.ctor name =<< mapM genExpr exprs

    Opt.CtorAccess expr index ->
      Pattern.ctorAccess index =<< genExpr expr

    Opt.Access record field ->
      Subst.one (BuiltIn.get field) =<< genExpr record

    Opt.Update record fields ->
      do  old <-
            genExpr record

          new <-
            genRecord fields

          Subst.many BuiltIn.update [new, old]

    Opt.Record fields ->
      genRecord fields

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
      genExpr expr

    Opt.GLShader _ _ _ ->
      error
        "TODO: remove shaders from AST"

    Opt.Crash _moduleName _region _maybeExpr ->
      error
        "TODO: Opt.Crash to Core.Expr"



--- VARIABLES


genVar :: Var.Canonical -> Env.Gen Core.Expr
genVar (Var.Canonical home name) =
  case home of
    Var.Local ->
      do  arity <-
            Env.getLocalArity name

          return $ maybe
            (Core.Lit (Core.LTerm (Core.Var name)))
            (genRef name)
            arity

    Var.Module moduleName ->
      genGlobal moduleName name

    Var.TopLevel moduleName ->
      genGlobal moduleName name

    Var.BuiltIn ->
      error
        "Will go away when merged with upstream dev."


genFunction :: Text -> [Text] -> Opt.Expr -> Env.Gen Core.Function
genFunction name args body =
  Env.withLocals (genLocals args) $
    Core.Function name args <$> genExpr body


genCall :: Opt.Expr -> [Core.Expr] -> Env.Gen Core.Expr
genCall function args =
  case function of
    Opt.Var (Var.Canonical (Var.Module moduleName) name)
      | ModuleName.canonicalIsNative moduleName ->
      genNative moduleName name args

    _ ->
      do  function' <-
            genExpr function

          case function' of
            Core.Lit f@(Core.LFunction _ arity)
              | arity == length args ->
              Subst.many (Core.Apply f) args

            _ ->
              Subst.many1 BuiltIn.apply function' args


genGlobal :: ModuleName.Canonical -> Text -> Env.Gen Core.Expr
genGlobal moduleName name =
  if ModuleName.canonicalIsNative moduleName then
    genNative moduleName name []

  else
    do  arity <-
          Env.getGlobalArity moduleName name

        return $ genRef (qualifiedVar moduleName name) arity


genRef :: Text -> Int -> Core.Expr
genRef name arity =
  if arity == 0 then
    Core.Apply (Core.LFunction name arity) []

  else
    Core.Lit (Core.LFunction name arity)


genNative
  :: ModuleName.Canonical
  -> Text
  -> [Core.Expr]
  -> Env.Gen Core.Expr
genNative (ModuleName.Canonical _ rawModule) name =
  Subst.many (Core.Call (Text.drop 7 rawModule) name)


genLet :: [Opt.Def] -> Opt.Expr -> Env.Gen Core.Expr
genLet defs expr =
  let
    toLocal name args _ =
      (name, Just (length args))
  in
    Env.withLocals (map (genDef toLocal) defs) $
      do  context <-
            genExpr expr

          functions <-
            mapM (genDef genFunction) defs

          return $ Core.LetRec functions context


genBinop :: Var.Canonical -> [Core.Expr] -> Env.Gen Core.Expr
genBinop (Var.Canonical home name) =
  Subst.many (Core.Apply (Core.LFunction qualified 2))

  where
    qualified =
      case home of
        Var.Local -> error "Will go away when merged with upstream dev"
        Var.Module moduleName -> qualifiedVar moduleName name
        Var.TopLevel moduleName -> qualifiedVar moduleName name
        Var.BuiltIn -> error "Will go away when merged with upstream dev"


genLocals :: [Text] -> [(Text, Maybe Int)]
genLocals =
  map (\name -> (name, Nothing))



-- RECORDS


genRecord :: [(Text, Opt.Expr)] -> Env.Gen Core.Expr
genRecord fields =
  do  let keys =
            map (Core.LTerm . Core.Atom . fst) fields

      values <-
        mapM (genExpr . snd) fields

      Subst.many (Core.Map . zip keys) values
