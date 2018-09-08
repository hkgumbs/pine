module Reporting.Render.Code exposing (render)

import Prelude exposing (last, maybe)
import Reporting.Helpers as H exposing (Doc)
import Reporting.Region as R


withHardline : Doc -> Doc -> Doc
withHardline a b =
    H.cat [ a, H.hardline, b ]


render : Maybe R.Region -> R.Region -> String -> Doc
render maybeSubRegion ((R.Region start end) as region) source =
    let
        (R.Position startLine _) =
            start

        (R.Position endLine _) =
            end

        relevantLines =
            (String.lines source ++ [ "" ])
                |> List.take endLine
                |> List.drop (startLine - 1)
                |> List.map2 (\a b -> ( a, b )) (List.range startLine (endLine + 1))

        width =
            last relevantLines
                |> Maybe.withDefault ( 0, "" )
                |> Tuple.first
                |> String.fromInt
                |> String.length

        smallerRegion =
            maybe region identity maybeSubRegion
    in
    case makeUnderline width endLine smallerRegion of
        Nothing ->
            drawLines True width smallerRegion relevantLines H.empty

        Just underline ->
            drawLines False width smallerRegion relevantLines underline


makeUnderline : Int -> Int -> R.Region -> Maybe Doc
makeUnderline width realEndLine (R.Region (R.Position start c1) (R.Position end c2)) =
    if start /= end || end < realEndLine then
        Nothing

    else
        let
            spaces =
                String.repeat (c1 + width + 1) " "

            zigzag =
                String.repeat (max 1 (c2 - c1)) "^"
        in
        Just (H.cat [ H.string spaces, H.dullred (H.string zigzag) ])


drawLines : Bool -> Int -> R.Region -> List ( Int, String ) -> Doc -> Doc
drawLines addZigZag width (R.Region start end) sourceLines finalLine =
    let
        (R.Position startLine _) =
            start

        (R.Position endLine _) =
            end
    in
    List.foldr withHardline finalLine <|
        List.map (drawLine addZigZag width startLine endLine) sourceLines


drawLine : Bool -> Int -> Int -> Int -> ( Int, String ) -> Doc
drawLine addZigZag width startLine endLine ( n, line ) =
    addLineNumber addZigZag width startLine endLine n (H.string line)


addLineNumber : Bool -> Int -> Int -> Int -> Int -> Doc -> Doc
addLineNumber addZigZag width start end n line =
    let
        number =
            if n < 0 then
                " "

            else
                String.fromInt n

        lineNumber =
            String.repeat (width - String.length number) " " ++ number ++ "|"

        spacer =
            if addZigZag && start <= n && n <= end then
                H.dullred (H.string ">")

            else
                H.string " "
    in
    H.cat [ H.string lineNumber, spacer, line ]
