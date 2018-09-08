module AST.Literal exposing
    ( GLType(..)
    , Literal(..)
    , Shader(..)
    , glTypeToVar
    , toString
    )

import AST.Module.Name as ModuleName
import AST.Variable as Var
import Dict exposing (Dict)
import Elm.Package as Pkg



-- LITERALS


type Literal
    = Chr Char
    | Str String
    | IntNum Int
    | FloatNum Float
    | Boolean Bool


toString : Literal -> String
toString literal =
    case literal of
        Chr c ->
            "'" ++ String.fromChar c ++ "'"

        Str s ->
            "\"" ++ s ++ "\""

        IntNum n ->
            String.fromInt n

        FloatNum n ->
            String.fromFloat n

        Boolean bool ->
            if bool then
                "True"

            else
                "False"



-- WebGL TYPES


type Shader
    = Shader
        { attribute : Dict String GLType
        , uniform : Dict String GLType
        , varying : Dict String GLType
        }


type GLType
    = Int
    | Float
    | V2
    | V3
    | V4
    | M4
    | Texture


glTypeToVar : GLType -> Var.Canonical
glTypeToVar glTipe =
    case glTipe of
        V2 ->
            inLinearAlgebra "Math.Vector2" "Vec2"

        V3 ->
            inLinearAlgebra "Math.Vector3" "Vec3"

        V4 ->
            inLinearAlgebra "Math.Vector4" "Vec4"

        M4 ->
            inLinearAlgebra "Math.Matrix4" "Mat4"

        Int ->
            Var.int

        Float ->
            Var.float

        Texture ->
            Var.fromModule (ModuleName.Canonical Pkg.webgl "WebGL") "Texture"


inLinearAlgebra : ModuleName.Raw -> String -> Var.Canonical
inLinearAlgebra moduleName tipe =
    Var.fromModule (ModuleName.Canonical Pkg.linearAlgebra moduleName) tipe
