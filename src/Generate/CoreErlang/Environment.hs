module Generate.CoreErlang.Environment
  ( Gen, run
  , getArity
  , freshName
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text
import Data.Map ((!))

import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Type as Type


type Gen a
  = State.State Env a


data Env = Env
  { _uid :: Int
  , _interfaces :: Module.Interfaces
  }


run :: Module.Interfaces -> Gen a -> a
run interfaces state =
  State.evalState state (Env 1 interfaces)


getArity :: ModuleName.Canonical -> Text.Text -> Gen Int
getArity moduleName name =
  do  (Env _ interfaces) <-
        State.get

      let tipe =
            Module.iTypes (interfaces ! moduleName) ! name

      return (Type.arity tipe)


freshName :: Gen Text.Text
freshName =
  do  (Env uid context) <- State.get
      State.put (Env (uid + 1) context)
      return (Text.pack (show uid))
