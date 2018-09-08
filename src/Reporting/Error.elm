module Reporting.Error exposing (Error(..), toJson, toReport)

-- TODO
-- import Reporting.Error.Canonicalize as Canonicalize
-- import Reporting.Error.Pattern as Pattern
-- import Reporting.Error.Type as Type

import Json.Encode as Json
import Prelude exposing (maybe)
import Reporting.Annotation as A
import Reporting.Error.Docs as Docs
import Reporting.Error.Syntax as Syntax
import Reporting.Region as Region
import Reporting.Render.Type as RenderType
import Reporting.Report as Report



-- ALL POSSIBLE ERRORS


type Error
    = Syntax Syntax.Error
    | Docs Docs.Error



-- TO REPORT


toReport : RenderType.Localizer -> Error -> Report.Report
toReport localizer err =
    case err of
        Syntax syntaxError ->
            Syntax.toReport localizer syntaxError

        Docs docsError ->
            Docs.toReport docsError



-- TO JSON


toJson : RenderType.Localizer -> String -> A.Located Error -> Json.Value
toJson localizer filePath (A.A region err) =
    let
        ( maybeRegion, additionalFields ) =
            case err of
                Syntax syntaxError ->
                    Report.toJson [] (Syntax.toReport localizer syntaxError)

                Docs docsError ->
                    Report.toJson [] (Docs.toReport docsError)
    in
    Json.object <|
        [ ( "file", Json.string filePath )
        , ( "region", Region.encode region )
        , ( "subregion", maybe Json.null Region.encode maybeRegion )
        , ( "type", Json.string "error" )
        ]
            ++ additionalFields
