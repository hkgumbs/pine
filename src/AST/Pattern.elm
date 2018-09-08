module AST.Pattern exposing (..)

import AST.Helpers as Help
import AST.Literal as L
import AST.Variable as Var
import Reporting.Annotation as A
import Reporting.Region as R
import Set


type alias Pattern ann var =
    A.Annotated ann (Pattern_ ann var)


type Pattern_ ann var
    = Ctor var (List (Pattern ann var))
    | Record (List String)
    | Alias String (Pattern ann var)
    | Var String
    | Anything
    | Literal L.Literal


type alias Raw =
    Pattern R.Region Var.Raw


type alias Raw_ =
    Pattern_ R.Region Var.Raw


type alias Canonical =
    Pattern R.Region Var.Canonical


isVar : String -> Pattern ann var -> Bool
isVar name (A.A _ pattern) =
    case pattern of
        Var pName ->
            name == pName

        _ ->
            False


list : R.Position -> List Raw -> Raw
list end patterns =
    case patterns of
        [] ->
            A.at end end (Ctor (Var.Raw "[]") [])

        ((A.A (R.Region start _) _) as pattern) :: rest ->
            A.at start end (Ctor (Var.Raw "::") [ pattern, list end rest ])


tuple : List Raw -> Raw_
tuple patterns =
    let
        name =
            Help.makeTuple (List.length patterns)
    in
    Ctor (Var.Raw name) patterns



-- FIND VARIABLES


boundVars : Pattern ann var -> List (A.Annotated ann String)
boundVars (A.A ann pattern) =
    case pattern of
        Var x ->
            [ A.A ann x ]

        Alias name realPattern ->
            A.A ann name :: boundVars realPattern

        Ctor _ patterns ->
            List.concatMap boundVars patterns

        Record fields ->
            List.map (A.A ann) fields

        Anything ->
            []

        Literal _ ->
            []


member : String -> Pattern ann var -> Bool
member name pattern =
    List.member name (List.map A.drop (boundVars pattern))


boundVarSet : Pattern ann var -> Set.Set String
boundVarSet pattern =
    Set.fromList (List.map A.drop (boundVars pattern))


boundVarList : Pattern ann var -> List String
boundVarList pattern =
    Set.toList (boundVarSet pattern)



-- TO STRING


toString : Canonical -> String
toString =
    toStringHelp identity


useParens : String -> String
useParens s =
    "(" ++ s ++ ")"


toStringHelp : (String -> String) -> Canonical -> String
toStringHelp withParens (A.A _ pattern) =
    case pattern of
        Var name ->
            name

        Ctor name [] ->
            if Var.isTuple name then
                "()"

            else
                Var.toString name

        Ctor name args ->
            if Var.isTuple name then
                useParens <|
                    String.join ", " (List.map (toStringHelp identity) args)

            else
                withParens <|
                    Var.toString name
                        ++ String.concat (List.map (\arg -> " " ++ toStringHelp useParens arg) args)

        Record fields ->
            "{" ++ String.join "," fields ++ "}"

        Alias alias_ subPattern ->
            withParens <|
                toStringHelp identity subPattern
                    ++ " as "
                    ++ alias_

        Anything ->
            "_"

        Literal literal ->
            L.toString literal
