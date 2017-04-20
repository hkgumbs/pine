module Generate.CoreErlang.Environment
  ( Gen, run
  , getGlobalArity
  , getLocalArity, withLocals
  , findNearest
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
  { uid :: Int
  , moduleName :: ModuleName.Canonical
  , interfaces :: Module.Interfaces
  , locals :: Map.Map Text.Text Arity
  }


run :: ModuleName.Canonical -> Module.Interfaces -> Gen a -> a
run moduleName interfaces state =
  State.evalState state (Env 1 moduleName interfaces Map.empty)


getGlobalArity :: ModuleName.Canonical -> Text.Text -> Gen Int
getGlobalArity moduleName name =
  do  globals <-
        State.gets interfaces

      let tipe =
            Module.iTypes (globals ! moduleName) ! name

      return (Type.arity tipe)



-- SCOPING RULES

{-| The Core Erlang AST technically has two types of variables:
    those with explicit arities (aka functions) and those without.
-}


type Arity = Maybe Int


getLocalArity :: Text.Text -> Gen Arity
getLocalArity name =
  (! name) <$> State.gets locals


withLocals :: [(Text.Text, Arity)] -> Gen a -> Gen a
withLocals new use =
  do  old <-
        State.get

      State.put $ old
        { locals = Map.union (Map.fromList new) (locals old)
        }

      result <-
        use

      State.put old >> return result


{-| Is the nearest variable with that name a global? -}

findNearest :: Text.Text -> Gen (Maybe ModuleName.Canonical)
findNearest name =
  do  inScope <-
        State.gets locals

      if Map.member name inScope
        then return Nothing
        else State.gets (Just . moduleName)



-- VARIABLES


freshName :: Gen Text.Text
freshName =
  do  old <-
        State.gets uid

      State.modify $ \env ->
        env { uid = old + 1 }

      return $ Text.pack (show old)
