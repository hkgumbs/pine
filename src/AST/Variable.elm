module AST.Variable exposing
    ( Canonical(..)
    , Global(..)
    , Home(..)
    , Raw(..)
    , bool
    , char
    , cmd
    , compareGlobals
    , cons
    , false
    , float
    , fromModule
    , inCore
    , inHtml
    , int
    , is
    , isArray
    , isCons
    , isJson
    , isKernel
    , isList
    , isLocal
    , isLocalHome
    , isMaybe
    , isPrim
    , isTask
    , isTuple
    , list
    , local
    , never
    , nil
    , rawToString
    , router
    , shader
    , string
    , sub
    , task
    , toString
    , topLevel
    , true
    , tuple
    )

import AST.Helpers as Help
import AST.Module.Name as ModuleName
import Elm.Package as Pkg



-- RAW NAMES


type Raw
    = Raw String



-- CANONICAL NAMES


type Home
    = BuiltIn
    | Module ModuleName.Canonical
    | TopLevel ModuleName.Canonical
    | Local


type Canonical
    = Canonical Home String



-- GLOBAL NAMES


type Global
    = Global ModuleName.Canonical String



-- HELPERS


local : String -> Canonical
local x =
    Canonical Local x


topLevel : ModuleName.Canonical -> String -> Canonical
topLevel home x =
    Canonical (TopLevel home) x


fromModule : ModuleName.Canonical -> String -> Canonical
fromModule home name =
    Canonical (Module home) name


inCore : ModuleName.Raw -> String -> Canonical
inCore home name =
    Canonical (Module (ModuleName.inCore home)) name


inHtml : ModuleName.Raw -> String -> Canonical
inHtml home name =
    Canonical (Module (ModuleName.inHtml home)) name


compareGlobals : Global -> Global -> Order
compareGlobals a b =
    let
        compareGlobalsHelp (Global (ModuleName.Canonical (Pkg.Name pkg version) raw) name) =
            ( pkg, version, ( raw, name ) )
    in
    compare (compareGlobalsHelp a) (compareGlobalsHelp b)



-- BUILT IN TYPES


int : Canonical
int =
    Canonical BuiltIn "Int"


float : Canonical
float =
    Canonical BuiltIn "Float"


char : Canonical
char =
    Canonical BuiltIn "Char"


string : Canonical
string =
    Canonical BuiltIn "String"


tuple : Int -> Canonical
tuple size =
    Canonical BuiltIn (Help.makeTuple size)



-- LIST


list : Canonical
list =
    Canonical BuiltIn "List"


cons : Canonical
cons =
    Canonical BuiltIn "::"


nil : Canonical
nil =
    Canonical BuiltIn "[]"



-- BOOLEANS


bool : Canonical
bool =
    Canonical BuiltIn "Bool"


true : Canonical
true =
    Canonical BuiltIn "True"


false : Canonical
false =
    Canonical BuiltIn "False"



-- CORE TYPES


never : Canonical
never =
    inCore "Basics" "Never"


task : Canonical
task =
    inCore "Platform" "Task"


cmd : Canonical
cmd =
    inCore "Platform.Cmd" "Cmd"


sub : Canonical
sub =
    inCore "Platform.Sub" "Sub"


router : Canonical
router =
    inCore "Platform" "Router"


shader : Canonical
shader =
    fromModule (ModuleName.Canonical Pkg.webgl "WebGL") "Shader"



-- VARIABLE RECOGNIZERS


isLocalHome : Home -> Bool
isLocalHome home =
    case home of
        BuiltIn ->
            False

        Module _ ->
            False

        TopLevel _ ->
            True

        Local ->
            True


isCons : Canonical -> Bool
isCons var =
    case var of
        Canonical BuiltIn "::" ->
            True

        _ ->
            False


is : ModuleName.Raw -> String -> Canonical -> Bool
is home name var =
    var == inCore home name


isJson : Canonical -> Bool
isJson =
    is "Json.Encode" "Value"


isMaybe : Canonical -> Bool
isMaybe =
    is "Maybe" "Maybe"


isArray : Canonical -> Bool
isArray =
    is "Array" "Array"


isTask : Canonical -> Bool
isTask =
    is "Task" "Task"


isList : Canonical -> Bool
isList var =
    var == Canonical BuiltIn "List"


isKernel : Canonical -> Bool
isKernel var =
    case var of
        Canonical (Module name) _ ->
            ModuleName.canonicalIsKernel name

        _ ->
            False


isTuple : Canonical -> Bool
isTuple var =
    case var of
        Canonical BuiltIn name ->
            Help.isTuple name

        _ ->
            False


isPrim : String -> Canonical -> Bool
isPrim prim (Canonical home name) =
    case home of
        BuiltIn ->
            name == prim

        _ ->
            False


isLocal : (String -> Bool) -> Canonical -> Bool
isLocal check (Canonical home name) =
    case home of
        Local ->
            check name

        _ ->
            False



-- VARIABLE TO TEXT


rawToString : Raw -> String
rawToString (Raw name) =
    name


toString : Canonical -> String
toString (Canonical home name) =
    case home of
        BuiltIn ->
            name

        Module moduleName ->
            ModuleName.canonicalToString moduleName ++ "." ++ name

        TopLevel _ ->
            name

        Local ->
            name
