module Reporting.Helpers
    exposing
        ( (<+>)
        , Doc
        , args
        , capitalize
        , cat
        , colon
        , comma
        , commaSep
        , display
        , distance
        , drawCycle
        , dullcyan
        , dullred
        , dullyellow
        , empty
        , equals
        , fillSep
        , findPotentialTypos
        , findTypoPairs
        , functionName
        , hang
        , hardline
        , hcat
        , hintLink
        , hsep
        , i2s
        , indent
        , lbrace
        , lparen
        , maybeYouWant
        , maybeYouWant_
        , moreArgs
        , nearbyNames
        , ordinalize
        , parens
        , plain
        , rbrace
        , reflowParagraph
        , rparen
        , sep
        , space
        , stack
        , string
        , toHint
        , underline
        , vcat
        , vetTypos
        )

import AST.Helpers as Help
import Char
import Dict
import Doc exposing ((|+))
import Elm.Compiler.Version as Compiler
import Elm.Package as Pkg
import Prelude exposing (init, last, maybe)
import Set
import StringDistance as Dist


-- DOC HELPERS


i2s : Int -> String
i2s =
    toString


functionName : String -> String
functionName opName =
    if Help.isOp opName then
        "(" ++ opName ++ ")"
    else
        "`" ++ opName ++ "`"


args : Int -> String
args n =
    i2s n
        ++ (if n == 1 then
                " argument"
            else
                " arguments"
           )


moreArgs : Int -> String
moreArgs n =
    i2s n
        ++ " more"
        ++ (if n == 1 then
                " argument"
            else
                " arguments"
           )



-- HINTS


toHint : String -> Doc
toHint str =
    let
        hint =
            cat [ underline (string "Hint"), string ":" ]
    in
    fillSep (hint :: List.map string (String.words str))


hintLink : String -> String
hintLink fileName =
    "<https://github.com/elm-lang/elm-compiler/blob/"
        ++ Pkg.versionToString Compiler.version
        ++ "/hints/"
        ++ fileName
        ++ ".md>"


stack : List Doc -> Doc
stack chunks =
    case chunks of
        [] ->
            Debug.crash "function `stack` requires a non-empty list of docs"

        doc :: docs ->
            List.foldl (\next acc -> cat [ acc, hardline, hardline, next ]) doc docs


reflowParagraph : String -> Doc
reflowParagraph paragraph =
    fillSep (List.map string (String.words paragraph))


commaSep : List String -> String
commaSep tokens =
    case tokens of
        [ token ] ->
            " " ++ token

        [ token1, token2 ] ->
            " " ++ token1 ++ " and " ++ token2

        _ ->
            " "
                ++ String.join ", " (init tokens)
                ++ ", and "
                ++ Maybe.withDefault "" (last tokens)


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( c, rest ) ->
            String.cons (Char.toUpper c) rest


ordinalize : Int -> String
ordinalize number =
    let
        remainder10 =
            number % 10

        remainder100 =
            number % 100

        ending =
            if List.member remainder100 (List.range 11 13) then
                "th"
            else if remainder10 == 1 then
                "st"
            else if remainder10 == 2 then
                "nd"
            else if remainder10 == 3 then
                "rd"
            else
                "th"
    in
    i2s number ++ ending


drawCycle : List String -> Doc
drawCycle names =
    let
        topLine =
            string "┌─────┐"

        nameLine name =
            cat
                [ string "│    "
                , dullyellow (string name)
                ]

        midLine =
            string "│     ↓"

        bottomLine =
            string "└─────┘"
    in
    vcat (topLine :: List.intersperse midLine (List.map nameLine names) ++ [ bottomLine ])



-- FIND TYPOS


findPotentialTypos : List String -> String -> List String
findPotentialTypos knownNames badName =
    List.filter ((>) 2.0 << distance badName) knownNames


findTypoPairs : List String -> List String -> List ( String, String )
findTypoPairs leftOnly rightOnly =
    let
        veryNear leftName =
            List.map ((,) leftName) (findPotentialTypos rightOnly leftName)
    in
    List.concatMap veryNear leftOnly


vetTypos : List ( String, String ) -> Maybe ( Set.Set String, Set.Set String )
vetTypos potentialTypos =
    let
        keysSet =
            Dict.keys >> Set.fromList

        tallyNames ( ln, rn ) ( lc, rc ) =
            ( Dict.update ln (Just << maybe 1 ((+) 1)) lc
            , Dict.update rn (Just << maybe 1 ((+) 1)) rc
            )

        ( leftCounts, rightCounts ) =
            List.foldr tallyNames ( Dict.empty, Dict.empty ) potentialTypos

        acceptable : Dict.Dict String Int -> Bool
        acceptable counts =
            not (Dict.isEmpty counts)
                && Dict.foldr (\_ n unique -> n < 2 && unique) True counts
    in
    if acceptable leftCounts && acceptable rightCounts then
        Just ( keysSet leftCounts, keysSet rightCounts )
    else
        Nothing



-- NEARBY NAMES


nearbyNames : (a -> String) -> a -> List a -> List a
nearbyNames format name names =
    let
        editDistance =
            if String.length (format name) < 3 then
                1
            else
                2
    in
    names
        |> List.map (\x -> ( distance (format name) (format x), x ))
        |> List.sortBy Tuple.first
        |> List.filter ((>) editDistance << abs << Tuple.first)
        |> List.map Tuple.second


distance : String -> String -> Float
distance =
    Dist.sift3Distance


maybeYouWant : Maybe Doc -> List String -> Doc
maybeYouWant maybeStarter suggestions =
    maybe empty identity (maybeYouWant_ maybeStarter suggestions)


maybeYouWant_ : Maybe Doc -> List String -> Maybe Doc
maybeYouWant_ maybeStarter suggestions =
    case suggestions of
        [] ->
            maybeStarter

        _ :: _ ->
            Just <|
                stack <|
                    [ maybe identity
                        (\s help -> cat [ s, space, help ])
                        maybeStarter
                        (string "Maybe you want one of the following?")
                    , indent 4 <|
                        vcat <|
                            List.map
                                (dullyellow << string)
                                (List.take 4 suggestions)
                    ]



-- DOC


type alias Doc =
    Doc.Doc



-- DISPLAY


display : Float -> Int -> Doc -> String
display fits width doc =
    Doc.display (Doc.renderPretty fits width doc)



-- CREATE


empty : Doc
empty =
    Doc.empty


hardline : Doc
hardline =
    Doc.hardline


space : Doc
space =
    Doc.space


string : String -> Doc
string =
    Doc.string


colon : Doc
colon =
    Doc.char ':'


comma : Doc
comma =
    Doc.char ','


equals : Doc
equals =
    Doc.char '='


lbrace : Doc
lbrace =
    Doc.char '{'


rbrace : Doc
rbrace =
    Doc.char '}'


lparen : Doc
lparen =
    Doc.char '('


rparen : Doc
rparen =
    Doc.char ')'



-- COMBINE


cat : List Doc -> Doc
cat =
    Doc.group << Doc.join Doc.linebreak


fillSep : List Doc -> Doc
fillSep =
    Doc.join Doc.softline


sep : List Doc -> Doc
sep =
    Doc.group << Doc.join Doc.line


hsep : List Doc -> Doc
hsep =
    Doc.join Doc.space


hcat : List Doc -> Doc
hcat =
    Doc.concat


vcat : List Doc -> Doc
vcat =
    Doc.join Doc.linebreak



-- MODIFIERS


indent : Int -> Doc -> Doc
indent =
    Doc.indent


hang : Int -> Doc -> Doc
hang =
    Doc.hang


parens : Doc -> Doc
parens =
    Doc.parens


dullred : Doc -> Doc
dullred =
    Doc.red


dullcyan : Doc -> Doc
dullcyan =
    Doc.cyan


dullyellow : Doc -> Doc
dullyellow =
    Doc.yellow


plain : Doc -> Doc
plain =
    Doc.plain


underline : Doc -> Doc
underline =
    Doc.underline


(<+>) : Doc -> Doc -> Doc
(<+>) doc1 doc2 =
    doc1
        |+ Doc.space
        |+ doc2
