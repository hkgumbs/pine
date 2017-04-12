{-# LANGUAGE OverloadedStrings #-}
module Pipeline.Generate (docs, generate) where

import Control.Monad.Except (forM_, liftIO)
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Tree as Tree
import Elm.Utils ((|>))
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import System.IO ( IOMode(WriteMode) )
import System.FilePath ((<.>))

import qualified BuildManager as BM
import qualified Path
import qualified TheMasterPlan as TMP
import qualified Utils.File as File



-- CONFIGURATION


beamModuleName :: String
beamModuleName =
  "elm"


compiledExtension :: String
compiledExtension =
  "core"



-- GENERATE DOCS


docs :: [Docs.Documentation] -> FilePath -> BM.Task ()
docs docsList path =
  Docs.prettyJson docsList
    |> LazyText.decodeUtf8
    |> LazyText.replace "\\u003e" ">"
    |> LazyText.writeFile path
    |> liftIO



-- GENERATE ELM STUFF

generate
  :: BM.Config
  -> Map.Map TMP.CanonicalModule Module.Interface
  -> Map.Map TMP.CanonicalModule [TMP.CanonicalModule]
  -> Map.Map TMP.CanonicalModule TMP.Location
  -> [TMP.CanonicalModule]
  -> BM.Task ()

generate _config _interfaces _dependencies _natives [] =
  return ()

generate config _interfaces dependencies _natives rootModules =
  do  let objectFiles =
            setupNodes (BM._artifactDirectory config) dependencies
              |> getReachableObjectFiles rootModules

          outputFile =
            beamModuleName <.> compiledExtension

      liftIO $
        File.withFileUtf8 outputFile WriteMode $ \handle ->
            do  BS.hPut handle (header (head rootModules))
                forM_ objectFiles $ \coreFile ->
                    BS.hPut handle =<< BS.readFile coreFile
                BS.hPut handle footer


setupNodes
  :: FilePath
  -> Map.Map TMP.CanonicalModule [TMP.CanonicalModule]
  -> [(FilePath, TMP.CanonicalModule, [TMP.CanonicalModule])]
setupNodes cachePath dependencies =
  Map.toList dependencies
    |> map (\(name, deps) -> (Path.toObjectFile cachePath name, name, deps))


getReachableObjectFiles
  :: [TMP.CanonicalModule]
  -> [(FilePath, TMP.CanonicalModule, [TMP.CanonicalModule])]
  -> [FilePath]
getReachableObjectFiles moduleNames nodes =
  let
    (dependencyGraph, vertexToKey, keyToVertex) =
      Graph.graphFromEdges nodes

    reachableSet =
      Maybe.mapMaybe keyToVertex moduleNames
        |> Graph.dfs dependencyGraph
        |> concatMap Tree.flatten
        |> Set.fromList
  in
    Graph.topSort dependencyGraph
      |> filter (\vtx -> Set.member vtx reachableSet)
      |> reverse
      |> map vertexToKey
      |> map (\(path, _, _) -> path)


header :: TMP.CanonicalModule -> BS.ByteString
header moduleName =
  let
    qualified =
      LazyText.encodeUtf8
        . LazyText.fromStrict
        . Module.qualifiedVar (TMP.simplifyModuleName moduleName)
  in
    BS.concat
      [ "module 'elm' ['module_info'/0, 'module_info'/1, 'main'/0] attributes []"
      , "\n'main'/0 = fun () ->"
      , "\n\tapply '", qualified "main", "'/0 ()"
      , "\n"
      ]

footer :: BS.ByteString
footer =
  BS.concat
    [ "\n'module_info'/0 ="
    , "\n    ( fun () ->"
    , "\n    ( call ( 'erlang'"
    , "\n       -| ['compiler_generated'] ):( 'get_module_info'"
    , "\n             -| ['compiler_generated'] )"
    , "\n    (( 'elm'"
    , "\n       -| ['compiler_generated'] ))"
    , "\n      -| ['compiler_generated'] )"
    , "\n      -| ['compiler_generated'] )"
    , "\n'module_info'/1 ="
    , "\n    ( fun (( _cor0"
    , "\n       -| ['compiler_generated'] )) ->"
    , "\n    ( call ( 'erlang'"
    , "\n       -| ['compiler_generated'] ):( 'get_module_info'"
    , "\n             -| ['compiler_generated'] )"
    , "\n    (( 'elm'"
    , "\n       -| ['compiler_generated'] ), ( _cor0"
    , "\n              -| ['compiler_generated'] ))"
    , "\n      -| ['compiler_generated'] )"
    , "\n      -| ['compiler_generated'] )"
    , "\nend"
    ]
