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
      generateFunction . qualifiedVar moduleName
  in
    Core.encodeUtf8 $ map
      (Env.run moduleName interfaces . generateDef topLevel)
      (Module.program info)


generateDef :: (Text -> [Text] -> Opt.Expr -> a) -> Opt.Def -> a
generateDef gen def =
    case def of
      Opt.Def _ name (Opt.Function args body) ->
        gen name args body

      Opt.Def _ name body ->
        gen name [] body

      Opt.TailDef _ name args body ->
        gen name args body


generateExpr :: Opt.Expr -> Env.Gen Core.Expr
generateExpr opt =
  case opt of
    Opt.Literal lit ->
      return $ Core.Lit (Core.LTerm (Literal.term lit))

    Opt.Var var ->
      generateVar var

    Opt.List exprs ->
      Pattern.list =<< mapM generateExpr exprs

    Opt.Binop var lhs rhs ->
      generateBinop var =<< mapM generateExpr [lhs, rhs]

    Opt.Function args body ->
      Env.withLocals (generateLocals args) $
        Core.Fun args <$> generateExpr body

    Opt.Call function args ->
      generateCall function =<< mapM generateExpr args

    Opt.TailCall name _ args ->
      do  moduleName <-
            Env.getModuleName

          let function =
                Core.LFunction (qualifiedVar moduleName name) (length args)

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
      generateLet defs expr

    Opt.Case switch branches ->
      let
        toCore (pattern, expr) =
          do  p <- Pattern.match pattern
              e <- generateExpr expr
              return (p, e)

        toLocals =
          generateLocals . concatMap (Pattern.names . fst)
      in
        Env.withLocals (toLocals branches) $
          do  branches' <-
                mapM toCore branches

              Subst.one (flip Core.Case branches') =<< generateExpr switch

    Opt.Ctor name exprs ->
      Pattern.ctor name =<< mapM generateExpr exprs

    Opt.CtorAccess expr index ->
      Pattern.ctorAccess index =<< generateExpr expr

    Opt.Access record field ->
      Subst.one (BuiltIn.get field) =<< generateExpr record

    Opt.Update record fields ->
      do  let zipper m entries =
                Core.Update (zip (generateKeys fields) entries) m

          record' <-
            generateExpr record

          Subst.many1 zipper record' =<< mapM (generateExpr . snd) fields

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
      error
        "TODO: remove shaders from AST"

    Opt.Crash _moduleName _region _maybeExpr ->
      error
        "TODO: Opt.Crash to Core.Expr"



--- VARIABLES


generateVar :: Var.Canonical -> Env.Gen Core.Expr
generateVar (Var.Canonical home name) =
  case home of
    Var.Local ->
      do  arity <-
            Env.getLocalArity name

          return $ maybe
            (Core.Lit (Core.LTerm (Core.Var name)))
            (generateRef name)
            arity

    Var.Module moduleName ->
      generateGlobal moduleName name

    Var.TopLevel moduleName ->
      generateGlobal moduleName name

    Var.BuiltIn ->
      error
        "Will go away when merged with upstream dev."


generateFunction :: Text -> [Text] -> Opt.Expr -> Env.Gen Core.Function
generateFunction name args body =
  Env.withLocals (generateLocals args) $
    Core.Function name args <$> generateExpr body


generateCall :: Opt.Expr -> [Core.Expr] -> Env.Gen Core.Expr
generateCall function args =
  case function of
    Opt.Var (Var.Canonical (Var.Module moduleName) name)
      | ModuleName.canonicalIsNative moduleName ->
      generateNative moduleName name args

    _ ->
      do  function' <-
            generateExpr function

          case function' of
            Core.Lit f@(Core.LFunction _ arity)
              | arity == length args ->
              Subst.many (Core.Apply f) args

            _ ->
              Subst.many1 BuiltIn.apply function' args


generateGlobal :: ModuleName.Canonical -> Text -> Env.Gen Core.Expr
generateGlobal moduleName name =
  if ModuleName.canonicalIsNative moduleName then
    generateNative moduleName name []

  else
    do  arity <-
          Env.getGlobalArity moduleName name

        return $ generateRef (qualifiedVar moduleName name) arity


generateRef :: Text -> Int -> Core.Expr
generateRef name arity =
  if arity == 0 then
    Core.Apply (Core.LFunction name arity) []

  else
    Core.Lit (Core.LFunction name arity)


generateNative
  :: ModuleName.Canonical
  -> Text
  -> [Core.Expr]
  -> Env.Gen Core.Expr
generateNative (ModuleName.Canonical _ rawModule) name =
  Subst.many (Core.Call (Text.drop 7 rawModule) name)


generateLet :: [Opt.Def] -> Opt.Expr -> Env.Gen Core.Expr
generateLet defs expr =
  let
    toLocal name args _ =
      (name, Just (length args))
  in
    Env.withLocals (map (generateDef toLocal) defs) $
      do  context <-
            generateExpr expr

          functions <-
            mapM (generateDef generateFunction) defs

          return $ Core.LetRec functions context


generateBinop :: Var.Canonical -> [Core.Expr] -> Env.Gen Core.Expr
generateBinop (Var.Canonical home name) =
  Subst.many (Core.Apply (Core.LFunction qualified 2))

  where
    qualified =
      case home of
        Var.Local -> error "Will go away when merged with upstream dev"
        Var.Module moduleName -> qualifiedVar moduleName name
        Var.TopLevel moduleName -> qualifiedVar moduleName name
        Var.BuiltIn -> error "Will go away when merged with upstream dev"


generateLocals :: [Text] -> [(Text, Maybe Int)]
generateLocals =
  map (\name -> (name, Nothing))



-- RECORDS


generateKeys :: [(Text, a)] -> [Core.Literal]
generateKeys =
  map (Core.LTerm . Core.Atom . fst)
