module Generate.CoreErlang.Environment
  ( Gen, run
  , getModuleName
  , getGlobalArity
  , getLocalArity, withLocals
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
  , _locals :: Map.Map Text.Text Arity
  }


run :: ModuleName.Canonical -> Module.Interfaces -> Gen a -> a
run moduleName interfaces state =
  State.evalState state (Env 1 moduleName interfaces Map.empty)


getModuleName :: Gen ModuleName.Canonical
getModuleName =
  State.gets _moduleName


getGlobalArity :: ModuleName.Canonical -> Text.Text -> Gen Int
getGlobalArity moduleName name =
  do  interfaces <-
        State.gets _interfaces

      let tipe =
            Module.iTypes (interfaces ! moduleName) ! name

      return (Type.arity tipe)



-- SCOPING RULES

{-| The Core Erlang AST technically has two types of variables:
    those with explicit arities (aka functions) and those without.
-}


type Arity = Maybe Int


getLocalArity :: Text.Text -> Gen Arity
getLocalArity name =
  (! name) <$> State.gets _locals


withLocals :: [(Text.Text, Arity)] -> Gen a -> Gen a
withLocals locals use =
  do  old <-
        State.get

      State.put $ old
        { _locals = Map.union (Map.fromList locals) (_locals old)
        }

      result <-
        use

      State.put old >> return result


freshName :: Gen Text.Text
freshName =
  do  uid <-
        State.gets _uid

      State.modify $ \env ->
        env { _uid = uid + 1 }

      return $ Text.pack (show uid)
