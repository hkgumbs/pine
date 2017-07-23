module AST.Helpers
    exposing
        ( desymbol
        , isOp
        , isSymbol
        , isTuple
        , makeTuple
        , zeroTuple
        )

import Char
import Dict exposing (Dict)


-- TUPLES


zeroTuple : String
zeroTuple =
    "#0"


makeTuple : Int -> String
makeTuple size =
    "#" ++ toString size


isTuple : String -> Bool
isTuple name =
    String.startsWith "#" name
        && String.all Char.isDigit (String.dropLeft 1 name)



-- INFIX OPERATORS


isOp : String -> Bool
isOp name =
    String.all isSymbol name


isSymbol : Char -> Bool
isSymbol c =
    Dict.member c validSymbols


validSymbols : Dict Char Char
validSymbols =
    Dict.fromList <|
        List.map2 (,)
            (String.toList "+-/*=.<>:&|^?%~!")
            (String.toList "abcdefghijklmnop")


desymbol : String -> String
desymbol name =
    let
        desymbolHelp c =
            Maybe.withDefault c (Dict.get c validSymbols)
    in
    String.map desymbolHelp name
