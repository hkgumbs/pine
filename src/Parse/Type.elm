module Parse.Type exposing (expression, unionConstructor)

import AST.Type as Type
import AST.Variable as Var
import Parse.Helpers exposing (..)
import Parse.Primitives exposing (..)
import Reporting.Annotation as A
import Reporting.Error.Syntax as E
import Reporting.Region as R



-- TYPE TERMS


term : Parser Type.Raw
term =
    let
        termHelp start =
            oneOf
                [ app0
                , variable start
                , tuple start
                , record start
                ]
    in
    hint E.Type <|
        andThen termHelp getPosition



-- TYPE VARIABLES


variable : R.Position -> Parser Type.Raw
variable start =
    succeed (\var end -> A.at start end (Type.RVar var))
        |= lowVar
        |= getPosition



-- TYPE EXPRESSIONS


expression : SParser Type.Raw
expression =
    hint E.Type <| andThen expressionStart getPosition


expressionStart : R.Position -> SParser Type.Raw
expressionStart start =
    oneOf
        [ app start
        , succeed (\a b c -> ( a, b, c ))
            |= term
            |= getPosition
            |= whitespace
        ]
        |> andThen (expressionEnd start)


expressionEnd : R.Position -> ( Type.Raw, R.Position, SPos ) -> SParser Type.Raw
expressionEnd start ( tipe1, end1, pos1 ) =
    oneOf
        [ succeed identity
            |. checkSpace pos1
            |. rightArrow
            |. spaces
            |= expression
            |> map
                (\( tipe2, end2, pos2 ) ->
                    ( A.at start end2 (Type.RLambda tipe1 tipe2)
                    , end2
                    , pos2
                    )
                )
        , succeed ( tipe1, end1, pos1 )
        ]



-- TYPE CONSTRUCTORS


app0 : Parser Type.Raw
app0 =
    let
        app0Help start name end =
            A.at start end (Type.RType (A.at start end (Var.Raw name)) [])
    in
    succeed app0Help
        |= getPosition
        |= qualifiedCapVar
        |= getPosition


app : R.Position -> SParser Type.Raw
app start =
    succeed (\a b -> ( a, b ))
        |= qualifiedCapVar
        |= getPosition
        |> andThen (appHelp start)


appHelp : R.Position -> ( String, R.Position ) -> SParser Type.Raw
appHelp start ( ctor, ctorEnd ) =
    let
        name =
            A.at start ctorEnd (Var.Raw ctor)
    in
    succeed identity
        |= whitespace
        |> andThen (eatArgs [] ctorEnd)
        |> map
            (\( args, end, pos ) ->
                ( A.at start end (Type.RType name args), end, pos )
            )


unionConstructor : SParser ( String, List Type.Raw )
unionConstructor =
    succeed (\a b c -> ( a, b, c ))
        |= capVar
        |= getPosition
        |= whitespace
        |> andThen unionConstructorHelp


unionConstructorHelp :
    ( String, R.Position, SPos )
    -> SParser ( String, List Type.Raw )
unionConstructorHelp ( ctor, ctorEnd, ctorSpace ) =
    eatArgs [] ctorEnd ctorSpace
        |> map (\( args, end, space ) -> ( ( ctor, args ), end, space ))


eatArgs : List Type.Raw -> R.Position -> SPos -> SParser (List Type.Raw)
eatArgs args end pos =
    oneOf
        [ succeed (\arg -> eatArgs (arg :: args))
            |. checkSpace pos
            |= term
            |= getPosition
            |= whitespace
            |> andThen identity
        , succeed ( List.reverse args, end, pos )
        ]



-- TUPLES


tuple : R.Position -> Parser Type.Raw
tuple start =
    skip leftParen <|
        inContext start E.TypeTuple <|
            oneOf
                [ succeed (\end -> Type.tuple (R.Region start end) [])
                    |. rightParen
                    |= getPosition
                , succeed identity
                    |. spaces
                    |= expression
                    |> andThen
                        (\( tipe, _, pos ) ->
                            succeed identity
                                |. checkSpace pos
                                |= tupleEnding start [ tipe ]
                        )
                ]


tupleEnding : R.Position -> List Type.Raw -> Parser Type.Raw
tupleEnding start tipes =
    oneOf
        [ succeed identity
            |. comma
            |. spaces
            |= andThen
                (\( tipe, _, pos ) ->
                    skip (checkSpace pos) (succeed tipe)
                )
                expression
            |> andThen
                (\tipe ->
                    tupleEnding start (tipe :: tipes)
                )
        , succeed identity
            |. rightParen
            |= getPosition
            |> map
                (\end ->
                    case List.reverse tipes of
                        [ tipe ] ->
                            tipe

                        tupleTypes ->
                            Type.tuple (R.Region start end) tupleTypes
                )
        ]



-- RECORD


record : R.Position -> Parser Type.Raw
record start =
    succeed identity
        |. leftCurly
        |. spaces
        |= recordHelp start


recordHelp : R.Position -> Parser Type.Raw
recordHelp start =
    inContext start E.TypeRecord <|
        oneOf
            [ succeed (\end -> A.at start end (Type.RRecord [] Nothing))
                |. rightCurly
                |= getPosition
            , addLocation lowVar
                |. spaces
                |> andThen (recordEnd start)
            ]


recordEnd : R.Position -> A.Located String -> Parser Type.Raw
recordEnd start var =
    oneOf
        [ let
            finish ( fields, end ) =
                Just (A.map Type.RVar var)
                    |> Type.RRecord fields
                    |> A.at start end

            getRemainingFields firstField =
                succeed (\a b -> ( a, b ))
                    |= chompFields [ firstField ]
                    |= getPosition
                    |> map finish
          in
          succeed identity
            |. pipe
            |. spaces
            |= field
            |> andThen getRemainingFields
        , let
            finish ( fields, end ) =
                Type.RRecord fields Nothing
                    |> A.at start end

            getRemainingFields ( tipe, _, nextPos ) =
                succeed (\a b -> ( a, b ))
                    |. checkSpace nextPos
                    |= chompFields [ ( var, tipe ) ]
                    |= getPosition
                    |> map finish
          in
          succeed identity
            |. hasType
            |. spaces
            |= expression
            |> andThen getRemainingFields
        ]


type alias Field =
    ( A.Located String, Type.Raw )


chompFields : List Field -> Parser (List Field)
chompFields fields =
    oneOf
        [ succeed (\f -> f :: fields)
            |. comma
            |. spaces
            |= field
            |> andThen chompFields
        , succeed (List.reverse fields)
            |. rightCurly
        ]


field : Parser Field
field =
    succeed identity
        |= addLocation lowVar
        |. spaces
        |. hasType
        |. spaces
        |> andThen (\name -> andThen (fieldHelp name) expression)


fieldHelp : A.Located String -> ( Type.Raw, R.Position, SPos ) -> Parser Field
fieldHelp name ( tipe, _, endPos ) =
    succeed ( name, tipe )
        |. checkSpace endPos
