module Optimize.Environment exposing
    ( Env
    , Optimizer
    , freshName
    , getHome
    , getTailCall
    , getVariantDict
    , indirectly
    , register
    , registerEffects
    , run
    , setTailCall
    )

import AST.Effects as Effects
import AST.Expression.Optimized as Opt
import AST.Module.Name as ModuleName
import AST.Variable as Var
import GenericSet exposing (GenericSet)
import Optimize.DecisionTree as DT
import State



-- ENVIRONMENT


type Env
    = Env
        { readOnlyVariants : DT.VariantDict
        , readOnlyHome : ModuleName.Canonical
        , uid : Int
        , deps : Deps
        , effects : Maybe Effects.ManagerType
        , hasTailCall : Bool
        }



-- DEPS


type Deps
    = Direct { direct : GenericSet Var.Global, indirect : GenericSet Var.Global }
    | Indirect (GenericSet Var.Global)


emptyGlobals : GenericSet Var.Global
emptyGlobals =
    GenericSet.empty Var.compareGlobals


emptyDeps : Deps
emptyDeps =
    Direct { direct = emptyGlobals, indirect = emptyGlobals }


insertDep : Var.Global -> Deps -> Deps
insertDep var deps =
    case deps of
        Direct { direct, indirect } ->
            Direct { direct = GenericSet.insert var direct, indirect = indirect }

        Indirect indirect ->
            Indirect (GenericSet.insert var indirect)


unionDeps : GenericSet Var.Global -> Deps -> Deps
unionDeps vars deps =
    case deps of
        Direct { direct, indirect } ->
            Direct { direct = GenericSet.union vars direct, indirect = indirect }

        Indirect indirect ->
            Indirect (GenericSet.union vars indirect)



-- OPTIMIZER


type alias Optimizer a =
    State.State Env a


run :
    DT.VariantDict
    -> ModuleName.Canonical
    -> Optimizer ( String, Opt.Def )
    -> ( String, Opt.Decl )
run variantDict home optimizer =
    let
        ( ( name, def ), Env { deps, effects } ) =
            State.run
                (Env
                    { readOnlyVariants = variantDict
                    , readOnlyHome = home
                    , uid = 0
                    , deps = emptyDeps
                    , effects = Nothing
                    , hasTailCall = False
                    }
                )
                optimizer
    in
    case deps of
        Direct { direct, indirect } ->
            ( name
            , Opt.Decl
                { direct = direct
                , indirect = indirect
                , effects = effects
                , body = def
                }
            )

        Indirect _ ->
            Debug.todo "Problem with Optimize.Environment#run"



-- READ ONLY


getVariantDict : Optimizer DT.VariantDict
getVariantDict =
    State.get |> State.map (\(Env e) -> e.readOnlyVariants)


getHome : Optimizer ModuleName.Canonical
getHome =
    State.get |> State.map (\(Env e) -> e.readOnlyHome)



-- TAIL CALLS


getTailCall : Optimizer Bool
getTailCall =
    State.get |> State.map (\(Env e) -> e.hasTailCall)


setTailCall : Bool -> Optimizer ()
setTailCall bool =
    State.modify (\(Env env) -> Env { env | hasTailCall = bool })



-- FRESH VARIABLES


freshName : Optimizer String
freshName =
    State.get
        |> State.andThen
            (\(Env env) ->
                State.put (Env { env | uid = env.uid + 1 })
                    |> State.map (\_ -> "_p" ++ toString env.uid)
            )



-- REGISTER DEPENDENCIES


register : Var.Global -> Optimizer ()
register global =
    State.modify (registerHelp global)


registerHelp : Var.Global -> Env -> Env
registerHelp dep (Env env) =
    Env { env | deps = insertDep dep env.deps }


indirectly : Optimizer a -> Optimizer a
indirectly optimizer =
    let
        indirectlyHelp direct result (Env env) =
            case env.deps of
                Indirect indirect ->
                    Env
                        { env
                            | deps =
                                Direct
                                    { direct = direct
                                    , indirect = indirect
                                    }
                        }
                        |> State.put
                        |> State.map (\_ -> result)

                Direct _ ->
                    Debug.todo "Problem with Optimize.Environment#indirectly"
    in
    State.get
        |> State.andThen
            (\(Env env) ->
                case env.deps of
                    Indirect _ ->
                        optimizer

                    Direct { direct, indirect } ->
                        State.put (Env { env | deps = Indirect indirect })
                            |> State.andThen (\_ -> optimizer)
                            |> State.andThen (\result -> State.andThen (indirectlyHelp direct result) State.get)
            )



-- REGISTER EFFECTS


registerEffects : ModuleName.Canonical -> Effects.ManagerType -> Optimizer Effects.ManagerType
registerEffects home manager =
    State.get
        |> State.andThen
            (\(Env env) ->
                State.put
                    (Env
                        { env
                            | effects = Just manager
                            , deps = unionDeps (getEffectDeps home manager) env.deps
                        }
                    )
                    |> State.map (\_ -> manager)
            )


getEffectDeps : ModuleName.Canonical -> Effects.ManagerType -> GenericSet Var.Global
getEffectDeps home manager =
    GenericSet.fromList Var.compareGlobals <|
        List.map (Var.Global home) <|
            case manager of
                Effects.Cmds ->
                    [ "init", "onEffects", "onSelfMsg", "cmdMap" ]

                Effects.Subs ->
                    [ "init", "onEffects", "onSelfMsg", "subMap" ]

                Effects.Both ->
                    [ "init", "onEffects", "onSelfMsg", "cmdMap", "subMap" ]
