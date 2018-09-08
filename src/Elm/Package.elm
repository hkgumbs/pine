module Elm.Package exposing
    ( Name(..)
    , Package(..)
    , Version(..)
    , bumpMajor
    , bumpMinor
    , bumpPatch
    , core
    , decoder
    , dummyName
    , dummyVersion
    , encode
    , encodeVersion
    , filterLatest
    , fromString
    , html
    , initialVersion
    , linearAlgebra
    , majorAndMinor
    , toFilePath
    , toString
    , toUrl
    , versionDecoder
    , versionToString
    , virtualDom
    , webgl
    )

import Char
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Prelude exposing (maybe)



-- PACKGE NAMES


type Name
    = Name String String


type Package
    = Package Name Version


dummyName : Name
dummyName =
    Name "user" "project"


core : Name
core =
    Name "elm-lang" "core"


virtualDom : Name
virtualDom =
    Name "elm-lang" "virtual-dom"


html : Name
html =
    Name "elm-lang" "html"


webgl : Name
webgl =
    Name "elm-community" "webgl"


linearAlgebra : Name
linearAlgebra =
    Name "elm-community" "linear-algebra"


toString : Name -> String
toString (Name user project) =
    user ++ "/" ++ project


toUrl : Name -> String
toUrl =
    toString


toFilePath : Name -> String
toFilePath =
    toString


fromString : String -> Result String Name
fromString string =
    case String.split "/" string of
        [ user, project ] ->
            fromStringHelp user project

        _ ->
            Err
                "A valid project name looks like `user/project`"


fromStringHelp : String -> String -> Result String Name
fromStringHelp user project =
    if not (String.isEmpty user || String.isEmpty project) then
        Result.map (Name user) (validateProjectName project)

    else
        Err
            "A valid project name looks like `user/project`"


validateProjectName : String -> Result String String
validateProjectName project =
    if String.contains "--" project then
        Err "There is a double dash -- in your package name. It must be a single dash."

    else if String.contains "_" project then
        Err "Underscores are not allowed in package names."

    else if String.contains "." project then
        Err "Dots are not allowed in package names."

    else if String.any Char.isUpper project then
        Err "Upper case characters are not allowed in package names."

    else if not (startsWithLetter project) then
        Err "Package names must start with a letter."

    else if String.endsWith "-" project then
        Err "Package names cannot end with a dash."

    else
        Ok project


startsWithLetter : String -> Bool
startsWithLetter str =
    let
        isLetter c =
            Char.isLower c || Char.isUpper c
    in
    maybe False (Tuple.first >> isLetter) (String.uncons str)



-- PACKAGE VERSIONS


type Version
    = Version Int Int Int


initialVersion : Version
initialVersion =
    Version 1 0 0


dummyVersion : Version
dummyVersion =
    Version 0 0 0


bumpPatch : Version -> Version
bumpPatch (Version major minor patch) =
    Version major minor (patch + 1)


bumpMinor : Version -> Version
bumpMinor (Version major minor _) =
    Version major (minor + 1) 0


bumpMajor : Version -> Version
bumpMajor (Version major _ _) =
    Version (major + 1) 0 0



-- FILTERING


filterLatest : (Version -> comparable) -> List Version -> List Version
filterLatest characteristic versions =
    List.sortWith compareVersions versions
        |> List.foldl (groupVersions characteristic) Dict.empty
        |> Dict.values
        |> List.filterMap List.head


groupVersions : (Version -> comparable) -> Version -> Dict comparable (List Version) -> Dict comparable (List Version)
groupVersions characteristic version =
    Dict.update (characteristic version) <|
        Just
            << maybe [ version ] ((::) version)


compareVersions : Version -> Version -> Order
compareVersions (Version major1 minor1 patch1) (Version major2 minor2 patch2) =
    compare ( major1, minor1, patch1 ) ( major2, minor2, patch2 )


majorAndMinor : Version -> ( Int, Int )
majorAndMinor (Version major minor patch) =
    ( major, minor )



-- CONVERSIONS


versionToString : Version -> String
versionToString (Version major minor patch) =
    String.join "."
        [ String.fromInt major
        , String.fromInt minor
        , String.fromInt patch
        ]


versionFromString : String -> Result String Version
versionFromString string =
    case String.split "." string of
        [ major, minor, patch ] ->
            Result.map3 Version
                (toNumber major)
                (toNumber minor)
                (toNumber patch)

        _ ->
            Err "Must have format MAJOR.MINOR.PATCH (e.g. 1.0.2)"


toNumber : String -> Result String Int
toNumber str =
    case String.toInt str of
        Just n ->
            Ok n

        Nothing ->
            Err "Must have format MAJOR.MINOR.PATCH (e.g. 1.0.2)"



-- JSON


decoder : Decode.Decoder Name
decoder =
    let
        decoderHelp str =
            case fromString str of
                Err msg ->
                    Decode.fail <| "Expecting a PACKAGE name. " ++ msg

                Ok name ->
                    Decode.succeed name
    in
    Decode.andThen decoderHelp Decode.string


encode : Name -> Encode.Value
encode name =
    Encode.string (toString name)


versionDecoder : Decode.Decoder Version
versionDecoder =
    let
        versionDecoderHelp str =
            case versionFromString str of
                Ok version ->
                    Decode.succeed version

                Err msg ->
                    Decode.fail <| "Expecting a VERSION. " ++ msg
    in
    Decode.andThen versionDecoderHelp Decode.string


encodeVersion : Version -> Encode.Value
encodeVersion version =
    Encode.string (versionToString version)
