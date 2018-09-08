module Reporting.Report exposing
    ( Pair
    , Report
    , report
    , toDoc
    , toJson
    )

import Json.Encode as Json
import Reporting.Helpers as Help
    exposing
        ( Doc
        , display
        , dullcyan
        , hardline
        , hcat
        , plain
        , string
        )
import Reporting.Region as R
import Reporting.Render.Code as Code



-- BUILD REPORTS


type Report
    = Report
        { title : String
        , highlight : Maybe R.Region
        , preHint : Doc
        , postHint : Doc
        }


report : String -> Maybe R.Region -> String -> Doc -> Report
report title highlight pre post =
    Report
        { title = title
        , highlight = highlight
        , preHint = Help.reflowParagraph pre
        , postHint = post
        }



-- REPORT TO JSON


type alias Pair =
    ( String, Json.Value )


toJson : List Pair -> Report -> ( Maybe R.Region, List Pair )
toJson extraFields (Report { title, highlight, preHint, postHint }) =
    let
        fields =
            [ ( "tag", Json.string title )
            , ( "overview", Json.string (nonAnsiRender preHint) )
            , ( "details", Json.string (nonAnsiRender postHint) )
            ]
    in
    ( highlight, fields ++ extraFields )


nonAnsiRender : Doc -> String
nonAnsiRender doc =
    display 1 80 (plain doc)



-- REPORT TO DOC


toDoc : String -> R.Region -> Report -> String -> Doc
toDoc location region (Report { title, highlight, preHint, postHint }) source =
    hcat
        [ messageBar title location
        , hardline
        , hardline
        , preHint
        , hardline
        , hardline
        , Code.render highlight region source
        , hardline
        , postHint
        , hardline
        , hardline
        ]


messageBar : String -> String -> Doc
messageBar tag location =
    let
        usedSpace =
            4 + String.length tag + 1 + String.length location
    in
    dullcyan <|
        string <|
            "-- "
                ++ tag
                ++ " "
                ++ String.repeat (max 1 (80 - usedSpace)) "-"
                ++ " "
                ++ location
