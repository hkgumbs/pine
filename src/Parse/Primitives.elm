module Parse.Primitives exposing
    ( Parser
    , SPos(..)
    , andThen
    , capVar
    , character
    , deadend
    , delayedCommit
    , delayedCommitMap
    , docComment
    , end
    , fail
    , getIndent
    , getPosition
    , hint
    , inContext
    , keyword
    , lazy
    , lowVar
    , map
    , map2
    , number
    , oneOf
    , run
    , string
    , succeed
    , symbol
    , underscore
    , whitespace
    )

import AST.Literal as L
import Parser.Advanced as P exposing ((|.), (|=))
import Reporting.Error.Syntax as E
    exposing
        ( Problem(..)
        , Theory(..)
        )
import Reporting.Region as R
import Set exposing (Set)



-- PARSER


type alias Parser a =
    P.Parser E.Context Problem a


run : Parser a -> String -> Result E.ParseError a
run parser source =
    case P.run parser source of
        Ok a ->
            Ok a

        Err [] ->
            Debug.todo "Parser failed without errors!"

        Err ({ row, col, contextStack, problem } :: _) ->
            Err <| E.ParseError row col (fixProblem contextStack problem)


fixProblem : List { row : Int, col : Int, context : E.Context } -> E.Problem -> E.Problem
fixProblem contextStack problem =
    case problem of
        Theories _ theories ->
            Theories (toContextStack contextStack) theories

        BadOp badOp _ ->
            BadOp badOp (toContextStack contextStack)

        _ ->
            problem


toContextStack : List { row : Int, col : Int, context : E.Context } -> E.ContextStack
toContextStack =
    List.map <| \x -> ( x.context, R.Position x.row x.col )



-- PRIMITIVES


succeed : a -> Parser a
succeed =
    P.succeed


fail : Problem -> Parser a
fail =
    P.problem



-- MAPPING


map : (a -> b) -> Parser a -> Parser b
map =
    P.map


map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 f first second =
    succeed f
        |= first
        |= second


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen =
    P.andThen



-- LAZY


lazy : (() -> Parser a) -> Parser a
lazy =
    P.lazy



-- ONE OF


oneOf : List (Parser a) -> Parser a
oneOf =
    P.oneOf



-- DELAYED COMMIT


delayedCommit : Parser a -> Parser value -> Parser value
delayedCommit =
    delayedCommitMap (\_ second -> second)


delayedCommitMap : (a -> b -> value) -> Parser a -> Parser b -> Parser value
delayedCommitMap f first second =
    succeed f
        |= P.backtrackable first
        |= second



-- TOKENS


symbol : String -> Parser ()
symbol str =
    P.token (P.Token str (Theories [] [ Symbol str ]))


keyword : String -> Parser ()
keyword str =
    P.token (P.Token str (Theories [] [ Keyword str ]))


underscore : Parser ()
underscore =
    keyword "_"



-- VARIABLES


lowVar : Parser String
lowVar =
    variable LowVar Char.isLower


capVar : Parser String
capVar =
    variable CapVar Char.isUpper


variable : Theory -> (Char -> Bool) -> Parser String
variable theory isFirst =
    P.variable
        { start = isFirst
        , inner = Char.isAlphaNum
        , reserved = keywords
        , expecting = Theories [] [ theory ]
        }


keywords : Set.Set String
keywords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]



-- NUMBER


number : Parser L.Literal
number =
    P.number
        { int = Ok L.IntNum
        , float = Ok L.FloatNum

        --
        , hex = Err BadNumberHex
        , octal = Err BadNumberEnd
        , binary = Err BadNumberEnd

        --
        , invalid = BadNumberEnd
        , expecting = BadNumberEnd
        }



-- WHITESPACE


type SPos
    = SPos R.Position


whitespace : Parser SPos
whitespace =
    succeed SPos
        |. P.spaces
        |= getPosition



-- DOCUMENTATION COMMENT


docComment : Parser String
docComment =
    lazy <| \() -> Debug.todo ""



-- STRINGS


string : Parser String
string =
    succeed identity
        |. symbol "\""
        |= P.getChompedString (P.chompUntil (P.Token "\"" NewLineInString))


character : Parser Char
character =
    succeed identity
        |. P.chompIf ((==) '\'') BadChar
        |= oneOf
            [ succeed identity
                |. P.chompIf ((==) '\\') BadChar
                |= oneOf
                    [ map (\_ -> '"') (P.chompIf ((==) '"') BadEscape)
                    , map (\_ -> '\n') (P.chompIf ((==) 'n') BadEscape)
                    , map (\_ -> '\t') (P.chompIf ((==) 't') BadEscape)
                    , map (\_ -> '\'') (P.chompIf ((==) '\'') BadEscape)
                    , map (\_ -> '\\') (P.chompIf ((==) '\\') BadEscape)
                    , map (\_ -> '\u{000D}') (P.chompIf ((==) 'r') BadEscape)
                    ]
                |. P.chompIf ((==) '\'') BadChar
            , P.getChompedString (P.chompIf (\_ -> True) (Theories [] []))
                |. P.chompIf ((==) '\'') BadChar
                |> andThen
                    (\s ->
                        case String.toList s of
                            [ '\'' ] ->
                                fail BadChar

                            [ '\n' ] ->
                                fail BadChar

                            [ c ] ->
                                succeed c

                            _ ->
                                fail BadChar
                    )
            ]



-- END


end : Parser ()
end =
    P.end (Theories [] [])



-- CONTEXT


deadend : List Theory -> Parser a
deadend =
    fail << Theories []


hint : E.Next -> Parser a -> Parser a
hint next parser =
    -- TODO [ E.Expecting next ]
    parser


inContext : E.Context -> Parser a -> Parser a
inContext =
    P.inContext



-- STATE


getPosition : Parser R.Position
getPosition =
    P.map (\( r, c ) -> R.Position r c) P.getPosition


getIndent : Parser Int
getIndent =
    P.getIndent
