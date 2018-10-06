{-# LANGUAGE OverloadedStrings #-}
module Generate.Llvm (generate) where

import qualified Data.Text.Lazy as LazyText

import LLVM.Pretty (ppllvm)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as Linkage
import qualified LLVM.AST.Visibility as Visibility
import qualified LLVM.AST.CallingConvention as Convention

import Prelude hiding (cycle, print)
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Interface as I
import qualified Elm.Name as N
import qualified Reporting.Doc as D
import qualified Reporting.Render.Type as RT
import qualified Reporting.Render.Type.Localizer as L



-- GENERATE MAINS


data Output
  = None
  | Some N.Name [N.Name] B.Builder


generate :: Mode.Mode -> Opt.Graph -> [ModuleName.Canonical] -> Output
generate mode (Opt.Graph mains graph _fields) roots =
  let
    rootSet = Set.fromList roots
    rootMap = Map.restrictKeys mains rootSet
  in
  case map ModuleName._module (Map.keys rootMap) of
    [] ->
      None

    name:names ->
      let
        state = Map.foldrWithKey (addMain mode graph) emptyState rootMap
        builder = perfNote mode <> stateToBuilder state <> toMainExports mode rootMap
      in
      Some name names builder


addMain :: Mode.Mode -> Graph -> ModuleName.Canonical -> main -> State -> State
addMain mode graph home _ state =
  addGlobal mode graph state (Opt.Global home "main")



-- GRAPH TRAVERSAL STATE


data State =
  State
    { _revKernels :: [B.Builder]
    , _revBuilders :: [B.Builder]
    , _seenGlobals :: Set.Set Opt.Global
    }


emptyState :: State
emptyState =
  State mempty [] Set.empty


stateToBuilder :: State -> B.Builder
stateToBuilder (State revKernels revBuilders _) =
  prependBuilders revKernels (prependBuilders revBuilders mempty)


prependBuilders :: [B.Builder] -> B.Builder -> B.Builder
prependBuilders revBuilders monolith =
  List.foldl' (\m b -> b <> m) monolith revBuilders



-- ADD DEPENDENCIES


type Graph = Map.Map Opt.Global Opt.Node


addGlobal :: Mode.Mode -> Graph -> State -> Opt.Global -> State
addGlobal mode graph state@(State revKernels builders seen) global =
  if Set.member global seen then
    state
  else
    addGlobalHelp mode graph global $
      State revKernels builders (Set.insert global seen)


addGlobalHelp :: Mode.Mode -> Graph -> Opt.Global -> State -> State
addGlocalHelp mode graph global state =
  -- TODO
  state


dummyLlvmModule :: AST.Module
dummyLlvmModule =
  AST.Module
    { AST.moduleName         = "pine"
    , AST.moduleDataLayout   = Nothing
    , AST.moduleTargetTriple = Nothing
    , AST.moduleDefinitions  =
        [ AST.GlobalDefinition
            (AST.Function
                Linkage.External
                Visibility.Default
                Nothing
                Convention.C
                []
                (AST.IntegerType 8)
                (AST.Name "main")
                ([], False)
                []
                Nothing
                Nothing
                0
                Nothing
                Nothing
                [ AST.BasicBlock
                    (AST.Name "entry")
                    []
                    (AST.Do
                        (AST.Ret
                            (Just (AST.ConstantOperand (AST.Int 8 42)))
                            []
                        )
                    )
                ]
            )
        ]
    }
