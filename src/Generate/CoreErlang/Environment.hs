module Generate.CoreErlang.Environment
  ( Gen, run
  , getGlobalArity, getLocalArity, withLocalArity
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
  , _interfaces :: Module.Interfaces
  , _locals :: Map.Map Text.Text Int
  }


run :: Module.Interfaces -> Gen a -> a
run interfaces state =
  State.evalState state (Env 1 interfaces Map.empty)


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


withLocalArity :: Text.Text -> Int -> Gen a -> Gen a
withLocalArity name arity use =
  do  (Env uid context locals) <- State.get
      State.put (Env uid context (Map.insert name arity locals))
      result <- use
      State.put (Env uid context locals)
      return result


freshName :: Gen Text.Text
freshName =
  do  (Env uid context locals) <- State.get
      State.put (Env (uid + 1) context locals)
      return (Text.pack (show uid))
