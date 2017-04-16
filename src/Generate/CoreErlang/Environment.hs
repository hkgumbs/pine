module Generate.CoreErlang.Environment
  ( Gen, run
  , freshName
  ) where


import qualified Control.Monad.State as State
import qualified Data.Text as Text

type Gen a
  = State.State Int a


run :: Gen a -> a
run state =
  State.evalState state 1


freshName :: Gen Text.Text
freshName =
  do  n <- State.get
      State.modify (+1)
      return (Text.pack (show n))
