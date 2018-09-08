module Parse.Helpers exposing
    ( (|.)
    , (|=)
    , SParser
    , addLocation
    , checkSpace
    , comma
    , cons
    , dot
    , equals
    , hasType
    , leftCurly
    , leftParen
    , leftSquare
    , pipe
    , qualifiedCapVar
    , qualifiedVar
    , rightArrow
    , rightCurly
    , rightParen
    , rightSquare
    , skip
    , spaces
    )

import Parse.Primitives as P exposing (Parser)
import Reporting.Annotation as A
import Reporting.Error.Syntax as E
    exposing
        ( BadOp(..)
        , ParseError(..)
        , Problem(..)
        , Theory(..)
        )
import Reporting.Region as R



-- PARSER


(|=) : Parser (a -> b) -> Parser a -> Parser b
(|=) funcParser nextParser =
    P.map2 (<|) funcParser nextParser


(|.) : Parser a -> Parser b -> Parser a
(|.) keepParser ignoreParser =
    P.map2 always keepParser ignoreParser


skip : Parser skip -> Parser a -> Parser a
skip =
    P.map2 (\_ a -> a)



-- SYMBOLS


equals : Parser ()
equals =
    P.symbol "="


dot : Parser ()
dot =
    P.symbol "."


comma : Parser ()
comma =
    P.symbol ","


pipe : Parser ()
pipe =
    P.symbol "|"


cons : Parser ()
cons =
    P.symbol "::"


hasType : Parser ()
hasType =
    P.symbol ":"


rightArrow : Parser ()
rightArrow =
    P.symbol "->"


leftParen : Parser ()
leftParen =
    P.symbol "("


rightParen : Parser ()
rightParen =
    P.symbol ")"


leftSquare : Parser ()
leftSquare =
    P.symbol "["


rightSquare : Parser ()
rightSquare =
    P.symbol "]"


leftCurly : Parser ()
leftCurly =
    P.symbol "{"


rightCurly : Parser ()
rightCurly =
    P.symbol "}"



-- VARIABLES


qualifiedVar : Parser String
qualifiedVar =
    P.oneOf
        [ P.lowVar
        , P.andThen (\var -> qualifiedVarHelp P.lowVar [ var ]) P.capVar
        ]


qualifiedCapVar : Parser String
qualifiedCapVar =
    P.andThen (\var -> qualifiedVarHelp (P.deadend [ E.CapVar ]) [ var ]) P.capVar


qualifiedVarHelp : Parser String -> List String -> Parser String
qualifiedVarHelp altEnding vars =
    P.oneOf
        [ let
            step () =
                P.oneOf
                    [ P.andThen (\var -> qualifiedVarHelp altEnding (var :: vars)) P.capVar
                    , P.map (\var -> String.join "." (List.reverse (var :: vars))) altEnding
                    ]
          in
          P.andThen step dot
        , P.succeed (String.join "." (List.reverse vars))
        ]



-- WHITESPACE


type alias SParser a =
    Parser ( a, R.Position, P.SPos )


spaces : Parser ()
spaces =
    P.andThen checkSpace P.whitespace


checkSpace : P.SPos -> Parser ()
checkSpace (P.SPos (R.Position _ col)) =
    let
        check indent =
            if col > indent && col > 1 then
                P.succeed ()

            else
                P.deadend [ E.BadSpace ]
    in
    P.andThen check P.getIndent



-- LOCATION


addLocation : Parser a -> Parser (A.Located a)
addLocation parser =
    P.succeed (\a b c -> ( a, b, c ))
        |= P.getPosition
        |= parser
        |= P.getPosition
        |> P.map (\( start, value, end ) -> A.at start end value)
