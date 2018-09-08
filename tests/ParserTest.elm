module ParserTest exposing (..)

import AST.Literal exposing (Literal(..))
import Expect exposing (Expectation)
import Parse.Primitives as P
import Reporting.Error.Syntax as E
import Reporting.Region as R
import Test exposing (..)


numbers : Test
numbers =
    describe "number parser" <|
        List.map (testRun P.number)
            [ p "0" Ok (IntNum 0)
            , p "12345" Ok (IntNum 12345)
            , p "6789 " Ok (IntNum 6789)
            , p "4e9" Ok (FloatNum 4.0e9)
            , p "3E7" Ok (FloatNum 3.0e7)
            , p "2e-9" Ok (FloatNum 2.0e-9)
            , p "1E+3" Ok (FloatNum 1.0e3)
            , p "3.14" Ok (FloatNum 3.14)
            , p "1.21  " Ok (FloatNum 1.21)
            , p "0.67" Ok (FloatNum 0.67)
            , p "6.022e26" Ok (FloatNum 6.022e26)
            , p "0.23E4" Ok (FloatNum 2.3e3)
            , p "0xFFFFFFFF" Ok (IntNum 0xFFFFFFFF)
            , p "0x0040" Ok (IntNum 0x40)
            , p "0xbeef" Ok (IntNum 0xBEEF)
            , p "0xBEEF" Ok (IntNum 0xBEEF)
            , p "0x0123789" Ok (IntNum 0x00123789)
            , p ".1234" Err (E.Theories [] [])
            , p "1." Err E.BadNumberDot
            , p "1_" Err E.BadNumberEnd
            , p "1.2_" Err E.BadNumberEnd
            , p "1e" Err E.BadNumberExp
            , p "1e." Err E.BadNumberExp
            , p "0x" Err E.BadNumberHex
            , p "0x7.5" Err E.BadNumberHex
            , p "0123" Err E.BadNumberZero
            ]


whitespace : Test
whitespace =
    describe "whitespace parser" <|
        List.map (testRun P.whitespace)
            [ p "" Ok (P.SPos (R.Position 1 1))
            , p "      " Ok (P.SPos (R.Position 1 7))
            , p "  \n" Ok (P.SPos (R.Position 2 1))
            , p "\n  " Ok (P.SPos (R.Position 2 3))
            , p "{--}" Ok (P.SPos (R.Position 1 5))
            , p "{--}\n" Ok (P.SPos (R.Position 2 1))
            , p "{-{--}-}" Ok (P.SPos (R.Position 1 9))
            , p "{-\n\n-}\n" Ok (P.SPos (R.Position 4 1))
            , p "-- asdf\n" Ok (P.SPos (R.Position 2 1))
            , p "--     \n  " Ok (P.SPos (R.Position 2 3))
            , p "123" Ok (P.SPos (R.Position 1 1))
            , p "{-|-}" Ok (P.SPos (R.Position 1 1))
            , p "\t" Err E.Tab
            , p " \t" Err E.Tab
            , p "{-" Err E.EndOfFile_Comment
            , p "{-\n\n" Err E.EndOfFile_Comment
            , p "{-{--}" Err E.EndOfFile_Comment
            ]


docComment : Test
docComment =
    describe "documentation comment parser" <|
        List.map (testRun P.docComment)
            [ p "{-|hello-}" Ok "hello"
            , p "{-|yo-}   " Ok "yo"
            , p "{-|{-|-}-}   " Ok "{-|-}"
            ]


string : Test
string =
    describe "string parser" <|
        List.map (testRun P.string)
            -- single
            [ p "\"hello world\"" Ok "hello world"
            , p "\"bye\\nworld\"" Ok "bye\nworld"
            , p "\"\"" Ok ""
            , p "\"'\"" Ok "'"
            , p "\"\\'\"" Ok "'"
            , p "\"\\t\"" Ok "\t"
            , p "\"" Err E.EndOfFile_String
            , p "\"\\\"" Err E.EndOfFile_String
            , p "\"\n\"" Err E.NewLineInString
            , p "\"\\x\"" Err E.BadEscape

            -- multi
            , p "\"\"\"hello world\"\"\"" Ok "hello world"
            , p "\"\"\"bye\nworld\"\"\"" Ok "bye\nworld"
            , p "\"\"\"bye\\nworld\"\"\"" Ok "bye\nworld"
            , p "\"\"\"\n\"\"\"" Ok "\n"
            , p "\"\"\"\\t\"\"\"" Ok "\t"
            , p "\"\"\"\"\"\"" Ok ""
            , p "\"\"\"'\"\"\"" Ok "'"
            , p "\"\"\"\\'\"\"\"" Ok "'"
            , p "\"\"\"\"\"" Err E.EndOfFile_String
            , p "\"\"\"\\\"\"\"" Err E.EndOfFile_String
            , p "\"\"\"\\x\"\"\"" Err E.BadEscape
            ]


character : Test
character =
    describe "character parser" <|
        List.map (testRun P.character)
            [ p "'a'" Ok 'a'
            , p "'\\n'" Ok '\n'
            , p "'\\''" Ok '\''
            , p "'''" Err E.BadChar
            , p "'  '" Err E.BadChar
            , p "'\n'" Err E.BadChar
            , p "'\\'" Err E.BadChar
            , p "'\\x'" Err E.BadEscape
            ]



-- HELPERS


testRun : P.Parser a -> ( String, Result E.Problem a ) -> Test.Test
testRun parser ( str, result ) =
    test ("can parse '" ++ str ++ "'") <|
        \_ ->
            P.run parser str
                |> Result.mapError (\(E.ParseError _ _ problem) -> problem)
                |> Expect.equal result


p : a -> (b -> c) -> b -> ( a, c )
p a f b =
    ( a, f b )
