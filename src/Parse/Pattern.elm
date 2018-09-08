module Parse.Pattern exposing (expression, term)

import AST.Literal as L
import AST.Pattern as P
import AST.Variable as Var
import Parse.Helpers exposing (..)
import Parse.Literal as Literal
import Parse.Primitives exposing (..)
import Reporting.Annotation as A
import Reporting.Error.Syntax as E
import Reporting.Region as R



-- PATTERN TERMS


term : Parser P.Raw
term =
    hint E.Pattern <|
        andThen termHelp getPosition


termHelp : R.Position -> Parser P.Raw
termHelp start =
    oneOf
        [ record start
        , tuple start
        , list start
        , succeed (\a b -> ( a, b ))
            |= oneOf
                [ map (\_ -> P.Anything) underscore
                , map P.Var lowVar
                , map mkCtor qualifiedCapVar
                , map P.Literal Literal.literal
                ]
            |= getPosition
            |> map (\( pattern, end ) -> A.at start end pattern)
        ]


mkCtor : String -> P.Raw_
mkCtor ctor =
    case ctor of
        "True" ->
            P.Literal (L.Boolean True)

        "False" ->
            P.Literal (L.Boolean False)

        _ ->
            P.Ctor (Var.Raw ctor) []



-- RECORDS


record : R.Position -> Parser P.Raw
record start =
    succeed identity
        |. leftCurly
        |= inContext start E.ExprRecord (recordStart start)


recordStart : R.Position -> Parser P.Raw
recordStart start =
    succeed identity
        |. spaces
        |= oneOf
            [ lowVar
                |. spaces
                |> andThen (\var -> recordEnd start [ var ])
            , succeed identity
                |. rightCurly
                |= getPosition
                |> map (\end -> A.at start end (P.Record []))
            ]


recordEnd : R.Position -> List String -> Parser P.Raw
recordEnd start vars =
    oneOf
        [ succeed identity
            |. comma
            |. spaces
            |= lowVar
            |. spaces
            |> andThen (\var -> recordEnd start (var :: vars))
        , succeed identity
            |. rightCurly
            |= getPosition
            |> map (\end -> A.at start end (P.Record vars))
        ]



-- TUPLES


tuple : R.Position -> Parser P.Raw
tuple start =
    succeed identity
        |. leftParen
        |= inContext start E.ExprTuple (tupleStart start)


tupleStart : R.Position -> Parser P.Raw
tupleStart start =
    succeed identity
        |. spaces
        |= oneOf
            [ expression
                |> andThen
                    (\( pattern, sPos ) ->
                        succeed identity
                            |. checkSpace sPos
                            |= tupleEnd start [ pattern ]
                    )
            , succeed identity
                |. rightParen
                |= getPosition
                |> map (\end -> A.at start end (P.tuple []))
            ]


tupleEnd : R.Position -> List P.Raw -> Parser P.Raw
tupleEnd start patterns =
    oneOf
        [ succeed identity
            |. comma
            |. spaces
            |= expression
            |> andThen
                (\( pattern, sPos ) ->
                    succeed identity
                        |. checkSpace sPos
                        |= tupleEnd start (pattern :: patterns)
                )
        , case patterns of
            [ pattern ] ->
                succeed pattern
                    |. rightParen

            _ ->
                succeed identity
                    |. rightParen
                    |= getPosition
                    |> map (\end -> A.at start end (P.tuple (List.reverse patterns)))
        ]



-- LIST


list : R.Position -> Parser P.Raw
list start =
    succeed identity
        |. leftSquare
        |= inContext start E.PatternList listStart


listStart : Parser P.Raw
listStart =
    succeed identity
        |. spaces
        |= oneOf
            [ expression
                |> andThen
                    (\( pattern, sPos ) ->
                        succeed identity
                            |. checkSpace sPos
                            |= listEnd [ pattern ]
                    )
            , succeed identity
                |. rightSquare
                |= getPosition
                |> map (\end -> P.list end [])
            ]


listEnd : List P.Raw -> Parser P.Raw
listEnd patterns =
    oneOf
        [ succeed identity
            |. comma
            |. spaces
            |= expression
            |> andThen
                (\( pattern, sPos ) ->
                    succeed identity
                        |. checkSpace sPos
                        |= listEnd (pattern :: patterns)
                )
        , getPosition
            |. rightSquare
            |> map (\end -> P.list end (List.reverse patterns))
        ]



-- PATTERN EXPRESSION


expression : Parser ( P.Raw, SPos )
expression =
    hint E.Pattern <|
        (succeed (\a b -> ( a, b ))
            |= getPosition
            |= consTerm
            |> andThen (\( start, cTerm ) -> exprHelp start [] cTerm)
        )


consTerm : SParser P.Raw
consTerm =
    oneOf
        [ succeed (\a b -> ( a, b ))
            |= getPosition
            |= qualifiedCapVar
            |> andThen
                (\( start, ctor ) ->
                    case ctor of
                        "True" ->
                            boolEnd start True

                        "False" ->
                            boolEnd start False

                        _ ->
                            constructorStart start ctor []
                )
        , succeed (\a b c -> ( a, b, c ))
            |= term
            |= getPosition
            |= whitespace
        ]


boolEnd : R.Position -> Bool -> SParser P.Raw
boolEnd start bool =
    succeed (\a b -> ( a, b ))
        |= getPosition
        |= whitespace
        |> map
            (\( end, sPos ) ->
                ( A.at start end <| P.Literal (L.Boolean bool), end, sPos )
            )


exprHelp : R.Position -> List P.Raw -> ( P.Raw, R.Position, SPos ) -> Parser ( P.Raw, SPos )
exprHelp start patterns ( pattern, end, sPos ) =
    oneOf
        [ succeed identity
            |. checkSpace sPos
            |. cons
            |. spaces
            |= andThen (exprHelp start (pattern :: patterns)) consTerm
        , succeed (\a b c -> ( a, b, c ))
            |. checkSpace sPos
            |. keyword "as"
            |. spaces
            |= lowVar
            |= getPosition
            |= whitespace
            |> map
                (\( alias_, newEnd, newSpace ) ->
                    ( A.at start newEnd <|
                        P.Alias alias_ (List.foldl (consHelp end) pattern patterns)
                    , newSpace
                    )
                )
        , succeed ( List.foldl (consHelp end) pattern patterns, sPos )
        ]


consHelp : R.Position -> P.Raw -> P.Raw -> P.Raw
consHelp end tl ((A.A (R.Region start _) _) as hd) =
    A.at start end (P.Ctor (Var.Raw "::") [ hd, tl ])


constructorStart : R.Position -> String -> List P.Raw -> SParser P.Raw
constructorStart start ctor args =
    succeed (\a b -> ( a, b ))
        |= getPosition
        |= whitespace
        |> andThen (\( a, b ) -> constructorEnd start ctor args a b)


constructorEnd : R.Position -> String -> List P.Raw -> R.Position -> SPos -> SParser P.Raw
constructorEnd start ctor args end sPos =
    oneOf
        [ succeed identity
            |. checkSpace sPos
            |= term
            |> andThen (\arg -> constructorStart start ctor (arg :: args))
        , succeed
            ( A.at start end <|
                P.Ctor (Var.Raw ctor) (List.reverse args)
            , end
            , sPos
            )
        ]
