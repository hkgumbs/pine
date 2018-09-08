module AST.Type exposing
    ( Aliased(..)
    , Canonical(..)
    , Raw
    , Raw_(..)
    , cmd
    , collectLambdas
    , dealias
    , deepDealias
    , iteratedDealias
    , sub
    , tuple
    )

import AST.Helpers as Help
import AST.Module.Name as ModuleName
import AST.Variable as Var
import Dict exposing (Dict)
import Reporting.Annotation as A
import Reporting.Region as R



-- DEFINITION


type alias Raw =
    A.Located Raw_


type Raw_
    = RLambda Raw Raw
    | RVar String
    | RType (A.Located Var.Raw) (List Raw)
    | RRecord (List ( A.Located String, Raw )) (Maybe Raw)


type Canonical
    = Lambda Canonical Canonical
    | Var String
    | Type Var.Canonical (List Canonical)
    | Record (List ( String, Canonical )) (Maybe Canonical)
    | Aliased Var.Canonical (List ( String, Canonical )) (Aliased Canonical)


type Aliased t
    = Holey t
    | Filled t



-- CONSTRUCT USEFUL TYPES


tuple : R.Region -> List Raw -> Raw
tuple region types =
    let
        name =
            Var.Raw (Help.makeTuple (List.length types))
    in
    A.A region (RType (A.A region name) types)


cmd : ModuleName.Canonical -> String -> Canonical
cmd =
    effect Var.cmd


sub : ModuleName.Canonical -> String -> Canonical
sub =
    effect Var.sub


effect : Var.Canonical -> ModuleName.Canonical -> String -> Canonical
effect effectName moduleName tipe =
    Lambda
        (Type (Var.fromModule moduleName tipe) [ Var "msg" ])
        (Type effectName [ Var "msg" ])



-- DEALIASING


deepDealias : Canonical -> Canonical
deepDealias tipe =
    case tipe of
        Lambda a b ->
            Lambda (deepDealias a) (deepDealias b)

        Var _ ->
            tipe

        Record fields ext ->
            Record
                (List.map (Tuple.mapSecond deepDealias) fields)
                (Maybe.map deepDealias ext)

        Aliased _ args tipe_ ->
            deepDealias (dealias args tipe_)

        Type name args ->
            Type name (List.map deepDealias args)


iteratedDealias : Canonical -> Canonical
iteratedDealias tipe =
    case tipe of
        Aliased _ args realType ->
            iteratedDealias (dealias args realType)

        _ ->
            tipe


dealias : List ( String, Canonical ) -> Aliased Canonical -> Canonical
dealias args aliasType =
    case aliasType of
        Holey tipe ->
            dealiasHelp (Dict.fromList args) tipe

        Filled tipe ->
            tipe


dealiasHelp : Dict String Canonical -> Canonical -> Canonical
dealiasHelp typeTable tipe =
    let
        go =
            dealiasHelp typeTable
    in
    case tipe of
        Lambda a b ->
            Lambda (go a) (go b)

        Var x ->
            Maybe.withDefault tipe (Dict.get x typeTable)

        Record fields ext ->
            Record (List.map (Tuple.mapSecond go) fields) (Maybe.map go ext)

        Aliased original args t_ ->
            Aliased original (List.map (Tuple.mapSecond go) args) t_

        Type name args ->
            Type name (List.map go args)



-- COLLECT LAMBDAS


collectLambdas : Canonical -> List Canonical
collectLambdas tipe =
    case tipe of
        Lambda arg result ->
            arg :: collectLambdas result

        _ ->
            [ tipe ]
