{-# OPTIONS_GHC -W #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Compiler (compilerTests) where

import Control.Exception (try, IOException)

import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Text.IO as Text
import Data.Text (Text)

import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>), (<.>), dropFileName, joinPath, splitDirectories)
import System.FilePath.Find (find, (==?), extension)

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertFailure, assertBool, assertEqual)

import qualified Elm.Compiler as Compiler
import qualified Elm.Package as Package
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Variable
import qualified AST.Type as Type


writeExpectedOutputEnvVarName :: String
writeExpectedOutputEnvVarName =
  "ELM_WRITE_NEW_EXPECTED"


compilerTests :: Test
compilerTests =
  buildTest $
    do  goods <- testIf isSuccess =<< getElms "good"
        bads  <- testIf isFailure =<< getElms "bad"
        expectedOutputTests <- testMatchesExpected "good" "good-expected-core"
        return $
            testGroup "Compile Tests"
              [ testGroup "Good Tests" goods
              , testGroup "Bad Tests"  bads
              , testGroup "Expected Output Tests" expectedOutputTests
              ]



-- GATHER FILES


getElms :: FilePath -> IO [FilePath]
getElms filePath =
    find
      (return True)
      (extension ==? ".elm")
      (testsDir </> filePath)


testsDir :: FilePath
testsDir =
    "tests" </> "test-files"

convertToExpectedOutputFilePath :: String -> FilePath -> FilePath
convertToExpectedOutputFilePath expectedOutputDir filePath =
    testsDir
      </> expectedOutputDir
      </> joinPath (drop 3 (splitDirectories filePath))
      <.> ".core"



-- TEST HELPERS


essentialInterfaces :: Module.Interfaces
essentialInterfaces =
  Map.fromList
    [
      (ModuleName.inCore "Debug",
        buildCoreInterface
          [
            ("crash", Type.Lambda (Type.Var "String") (Type.Var "a"))
          ]
      )
    ]


buildCoreInterface :: [(Text, Type.Canonical)] -> Module.Interface
buildCoreInterface members =
  let
    exports = map (Variable.Value . fst) members
    imports = []
    types = Map.fromList members
    adts = Map.empty
    aliases = Map.empty
    fixities = []
  in
    Module.Interface Compiler.version Package.core exports imports types adts aliases fixities


compile :: FilePath -> Text -> Either String Compiler.Result
compile filePath source =
  let dependentModuleNames =
        Map.keys essentialInterfaces
      context =
        Compiler.Context Package.core False dependentModuleNames
      (dealiaser, _warnings, result) =
        Compiler.compile context source essentialInterfaces
      formatErrors errors =
        concatMap (Compiler.errorToString dealiaser filePath source) errors
  in
      either (Left . formatErrors) Right result


readFileOrErrorStr :: FilePath -> IO String
readFileOrErrorStr filePath =
  do  strOrExc <- try $ readFile filePath
      case strOrExc of
        Right s ->
          return s

        Left (e :: IOException) ->
          return (show e)



-- RUN TESTS


testIf
    :: (Either String Compiler.Result -> Assertion)
    -> [FilePath]
    -> IO [Test]
testIf handleResult filePaths =
  let doTest filePath =
        do  source <- Text.readFile filePath
            let formattedResult = compile filePath source
            return $ testCase filePath (handleResult formattedResult)
  in
      traverse doTest filePaths


doMatchesExpectedTest :: String -> FilePath -> IO Test
doMatchesExpectedTest expectedOutputDir filePath =
  do  source <- Text.readFile filePath
      expectedOutput <- readFileOrErrorStr (convertToExpectedOutputFilePath expectedOutputDir filePath)

      let formattedResult = compile filePath source
      let assertion = matchesExpected expectedOutput formattedResult

      return $ testCase filePath assertion


doWriteNewExpectedTest :: String -> FilePath -> IO Test
doWriteNewExpectedTest expectedOutputDir filePath =
  do  source <- Text.readFile filePath
      let formattedResult = compile filePath source
      let expectedFilePath = convertToExpectedOutputFilePath expectedOutputDir filePath
      let assertion =
            case formattedResult of
                Right (Compiler.Result _ _ core) ->
                  do  createDirectoryIfMissing True (dropFileName expectedFilePath)
                      -- Force the evaluation of `core` before `writeFile`
                      --  so that if an error is raised we do not unintentionally write an empty file.
                      --  NB: `core` can be an empty string in the case of NoExpressions.elm
                      assertBool "" (LText.length core >= 0)
                      LText.writeFile expectedFilePath core
                      assertFailure ("Wrote new expected core: " ++ expectedFilePath)

                _ ->
                  assertFailure "Compile failed. Could not write new expected core."
      return $ testCase filePath assertion


testMatchesExpected :: String -> String -> IO [Test]
testMatchesExpected elmDir expectedOutputDir =
  do  -- TODO: elmFiles <- getElms elmDir
      envVar <- lookupEnv writeExpectedOutputEnvVarName
      if isJust envVar
        then
          traverse (doWriteNewExpectedTest expectedOutputDir) elmFiles
        else
          traverse (doMatchesExpectedTest expectedOutputDir) elmFiles
  where
      elmFiles = map ((</>) $ testsDir </> elmDir)
          [ "Literals" </> "Numbers.elm"
          , "Literals" </> "Negatives.elm"
          , "Literals" </> "Characters.elm"
          , "Literals" </> "Multiline.elm"
          , "Literals" </> "QuotesAndComments.elm"
          , "Literals" </> "Unicode.elm"
          , "Lists" </> "EmptyList.elm"
          , "Lists" </> "NonEmptyList.elm"
          , "Cases" </> "CharCase.elm"
          , "Cases" </> "ListZip.elm"
          , "Cases" </> "ListPattern.elm"
          , "Functions" </> "Simple.elm"
          , "Functions" </> "AsArgument.elm"
          , "Functions" </> "Native.elm"
          , "Functions" </> "AliasPattern.elm"
          , "Declarations" </> "Types.elm"
          , "CodeGenBugs" </> "CommentInterpretedAsInfix.elm"
          , "Soundness" </> "Apply.elm"
          ]



-- CHECK RESULTS


isSuccess :: Either String a -> Assertion
isSuccess result =
    case result of
      Right _ ->
          assertBool "" True

      Left errorMessages ->
          assertFailure errorMessages


isFailure :: Either a b -> Assertion
isFailure result =
    case result of
      Right _ ->
          assertFailure "Compilation succeeded but should have failed"

      Left _ ->
          assertBool "" True


matchesExpected :: String -> Either String Compiler.Result -> Assertion
matchesExpected expectedOutput result =
    case result of
      Right (Compiler.Result _ _ core) ->
          assertEqual matchFailureMessage (LText.pack expectedOutput) core

      Left errorMessages ->
          assertFailure $ "Compile failed:\n" ++ errorMessages


matchFailureMessage :: String
matchFailureMessage =
  "Compiled Output did not match expected Output." ++
  "\n  If the change is intentional, rerun " ++
  "the tests with environment variable: " ++ writeExpectedOutputEnvVarName ++ "=1"
