module Elm.Compiler exposing (Tag(..), parseHeader, toHeaderSummary)

import AST.Module as Module
import Parse.Helpers as Parse


type Tag
    = Normal
    | Effect
    | Port


parseHeader : Package.Name -> Text -> Either Error ( Tag, Maybe M.Raw, List M.Raw )
parseHeader pkgName sourceCode =
    case Parse.run Parse.header sourceCode of
        Right header ->
            Right <| toHeaderSummary pkgName header

        Left err ->
            Left (Error (A.map Error.Syntax err))


toHeaderSummary : Package.Name -> Module.Header (List Module.UserImport) -> ( Tag, Maybe M.Raw, List M.Raw )
toHeaderSummary pkgName (Module.Header maybeHeaderDecl imports) =
    let
        dependencies =
            if pkgName == Package.core then
                map (A.drop . fst . A.drop) imports

            else
                map (A.drop . fst . A.drop) imports ++ map fst Imports.defaults
    in
    case maybeHeaderDecl of
        Nothing ->
            ( Normal, Nothing, dependencies )

        Just (Module.HeaderDecl sourceTag name _ _ _) ->
            let
                tag =
                    case sourceTag of
                        Module.Normal ->
                            Normal

                        Module.Port _ ->
                            Port

                        Module.Effect _ ->
                            Effect
            in
            ( tag, Just name, dependencies )
