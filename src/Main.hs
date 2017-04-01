{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (foldM)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Builder as BS
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8Builder)

import qualified System.Environment as Env
import qualified System.IO as IO
import qualified System.Directory as Dir
import System.Exit (exitFailure)
import System.FilePath ((<.>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package as Package


help :: String
help =
  "elm-beam"
  ++ "\nNaive tool to build small elm projects"
  ++ "\n"
  ++ "\nUsage:"
  ++ "\n  elm-beam [[--src-dir DIR]] FILE"
  ++ "\n"
  ++ "\nExamples:"
  ++ "\n  elm-beam --src-dir . Main.elm"
  ++ "\n  elm-beam --src-dir src --src-dir tests tests/Test.elm"


main :: IO ()
main =
  do  args <-
        Env.getArgs

      almostModules <-
        maybe
          (putStrLn help >> exitFailure)
          (uncurry collectModules)
          (initEnv [] args)

      case compile =<< almostModules of
        Left errors ->
          do  mapM_ printError errors
              exitFailure

        Right core ->
          do  IO.withFile "elm.core" IO.WriteMode (flip BS.hPutBuilder core)
              putStrLn "Successfully generated elm.core"



-- GET DEPENDENCIES


type ErrorsOr a =
  Either [(Text.Text, Compiler.Error)] a


initEnv :: [FilePath] -> [String] -> Maybe ([FilePath], FilePath)
initEnv srcDirs args =
  case args of
    "--src-dir" : dir : otherArgs ->
      initEnv (dir : srcDirs) otherArgs

    [file] ->
      Just (srcDirs, file)

    _ ->
      Nothing


collectModules
  :: [FilePath]
  -> FilePath
  -> IO (ErrorsOr [(Module.Canonical, Text.Text)])
collectModules srcDirs file =
  do  code <-
        TextIO.readFile file

      let collect rawModule =
            (Module.Canonical Package.core rawModule, code)

          fileSearch =
            Dir.findFiles srcDirs . (<.> "elm") . Module.nameToPath

          findDeps rawDeps =
            do  allDepFiles <-
                  mapM fileSearch rawDeps

                foldM
                  (\acc -> combineEithers acc . collectModules srcDirs)
                  (Right [])
                  (concat allDepFiles)

      either
        (\e -> return (Left [(code, e)]))
        (\(_, mod, deps) -> fmap (collect mod :) <$> findDeps deps)
        (Compiler.parseDependencies Package.core code)



-- COMPILE!


compile :: [(Module.Canonical, Text.Text)] -> ErrorsOr BS.Builder
compile modules =
  modules
    |> reverse
    |> List.nub
    |> foldM compileHelp (Map.empty, "")
    |> fmap (\(_, bs) -> coreModule (fst (head modules)) bs)


compileHelp
  :: (Module.Interfaces, BS.Builder)
  -> (Module.Canonical, Text.Text)
  -> ErrorsOr (Module.Interfaces, BS.Builder)
compileHelp (interfaces, compiled) (moduleName, code) =
  do  let context =
            Compiler.Context Package.core False (Map.keys interfaces)

          (_, _, result) =
            Compiler.compile context code interfaces

      (Compiler.Result _ iface core) <-
        either (Left . map ((,) code)) Right result

      return (Map.insert moduleName iface interfaces, mappend core compiled)


printError :: (Text.Text, Compiler.Error) -> IO ()
printError (code, e) =
  Compiler.printError IO.stdout Compiler.dummyLocalizer "" code e



-- EXTRA CODEGEN STUFF


coreModule :: Module.Canonical -> BS.Builder -> BS.Builder
coreModule moduleName compiledFunctions =
  let
    qualified =
      encodeUtf8Builder . Module.qualifiedVar moduleName
  in
    mconcat
      [ "module 'elm' ['module_info'/0, 'module_info'/1, 'main'/0] attributes []"
      , "\n'main'/0 = fun () ->"
      , "\n\tapply '", qualified "main", "'/0 ()"
      , "\n", compiledFunctions
      , "\n'module_info'/0 ="
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



-- UTILITY


combineEithers
  :: Monad m
  => Either [a] [b]
  -> m (Either [a] [b])
  -> m (Either [a] [b])
combineEithers acc new =
  do  n <- new
      return $
        case (acc, n) of
          (Left accAs, Left newAs)   -> Left  (accAs ++ newAs)
          (Left accAs, Right _)      -> Left  (accAs)
          (Right _, Left newAs)      -> Left  (newAs)
          (Right accBs, Right newBs) -> Right (accBs ++ newBs)


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a
