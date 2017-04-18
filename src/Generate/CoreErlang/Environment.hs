module Generate.CoreErlang.Environment
  ( Gen, run
  , getModuleName
  , getGlobalArity, getLocalArity, putLocalArity
  , freshName
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Map ((!))

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type


type Gen a
  = State.State Env a


data Env = Env
  { _uid :: Int
  , _moduleName :: ModuleName.Canonical
  , _interfaces :: Module.Interfaces
  , _locals :: Map.Map Text.Text Int
  }


run :: ModuleName.Canonical -> Module.Interfaces -> Gen a -> a
run moduleName interfaces state =
  State.evalState state (Env 1 moduleName interfaces Map.empty)


getModuleName :: Gen ModuleName.Canonical
getModuleName =
  _moduleName <$> State.get


getGlobalArity :: ModuleName.Canonical -> Text.Text -> Gen Int
getGlobalArity moduleName name =
  do  interfaces <-
        _interfaces <$> State.get

      let tipe =
            Module.iTypes (interfaces ! moduleName) ! name

      return (Type.arity tipe)


getLocalArity :: Text.Text -> Gen (Maybe Int)
getLocalArity name =
  do  locals <-
        _locals <$> State.get

      return (Map.lookup name locals)


putLocalArity :: Text.Text -> Int -> Gen ()
putLocalArity name arity =
  do  (Env uid moduleName context locals) <- State.get
      State.put (Env uid moduleName context (Map.insert name arity locals))
      return ()


freshName :: Gen Text.Text
freshName =
  do  (Env uid moduleName context locals) <- State.get
      State.put (Env (uid + 1) moduleName context locals)
      return (Text.pack (show uid))
