module Parse.Repl exposing
    ( Entry(..)
    , parseEntry
    )

import AST.Pattern as P
import Parse.Helpers exposing (..)
import Parse.Pattern as Pattern
import Parse.Primitives exposing (..)
import Reporting.Annotation as A


type Entry
    = Import String String
    | Type String String
    | Def (Maybe String) String
    | Other String
    | Annotation
    | Port
    | Infix


parseEntry : String -> Entry
parseEntry source =
    case run (entryParser source) source of
        Ok entry ->
            entry

        Err _ ->
            Other source


entryParser : String -> Parser Entry
entryParser source =
    oneOf
        [ succeed (\name -> Import name source)
            |. keyword "import"
            |. spaces
            |= qualifiedCapVar
        , succeed Infix
            |. oneOf (List.map keyword [ "infix", "infixl", "infixr" ])
        , succeed Port
            |. keyword "port"
        , succeed (\name -> Type name source)
            |. keyword "type"
            |. spaces
            |. oneOf
                [ keyword "alias" |. spaces
                , succeed ()
                ]
            |= capVar
        , Pattern.term
            |. spaces
            |> andThen (termToEntry source)
        ]


termToEntry : String -> P.Raw -> Parser Entry
termToEntry source root =
    case A.drop root of
        P.Var name ->
            oneOf
                [ succeed Annotation
                    |. hasType
                , succeed (Def (Just name) source)
                    |. chompArgs
                ]

        _ ->
            succeed (Def Nothing source)
                |. chompArgs


chompArgs : Parser ()
chompArgs =
    oneOf
        [ succeed ()
            |. Pattern.term
            |. spaces
            |. lazy (\_ -> chompArgs)
        , equals
        ]
