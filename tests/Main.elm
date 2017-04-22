port module Main exposing (..)

import Basics exposing (..)
import Task exposing (..)
import Test exposing (..)
import Platform
import Platform.Cmd exposing (Cmd)
import Platform.Sub as Sub
import Json.Decode exposing (Value)
import Test.Array as Array
import Test.Basics as Basics
import Test.Bitwise as Bitwise
import Test.Char as Char
import Test.CodeGen as CodeGen
import Test.Dict as Dict
import Test.Maybe as Maybe
import Test.Equality as Equality
import Test.Json as Json
import Test.List as List
import Test.Result as Result
import Test.Set as Set
import Test.String as String
import Test.Regex as Regex
import Test.Tuple as Tuple

import Runner.Log


tests : Test
tests =
    describe "Elm Standard Library Tests"
        [ Array.tests
        , Basics.tests
        , Bitwise.tests
        , Char.tests
        , CodeGen.tests
        , Dict.tests
        , Equality.tests
        , Json.tests
        , List.tests
        , Result.tests
        , Set.tests
        , String.tests
        , Regex.tests
        , Maybe.tests
        , Tuple.tests
        ]


main : Platform.Program Never () msg
main =
    Platform.program
        { init = ((), Platform.Cmd.none)
        , update = \_ _ -> ((), Platform.Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
        |> Runner.Log.run tests
