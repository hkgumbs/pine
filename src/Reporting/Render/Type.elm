module Reporting.Render.Type exposing
    ( Diff(..)
    , Localizer
    , Style(..)
    , annotation
    , decl
    , diffToDocs
    , sequenceDiff
    , toDoc
    )

import AST.Helpers as Help
import AST.Type as Type
import AST.Variable as Var
import Dict
import Dict.Any as GenericDict
import Prelude exposing (mapBoth, maybe, repeat)
import Reporting.Helpers as Help exposing (Doc, cat, dullyellow, hang, hsep, parens, sep, string, vcat, withSpace)
import Set



-- PUBLIC API FOR CREATING DOCS


toDoc : Localizer -> Type.Canonical -> Doc
toDoc localizer tipe =
    docType localizer None tipe


diffToDocs : Localizer -> Type.Canonical -> Type.Canonical -> ( Doc, Doc )
diffToDocs localizer leftType rightType =
    case diff localizer None leftType rightType of
        Same doc ->
            ( doc, doc )

        Diff leftDoc rightDoc ->
            ( leftDoc, rightDoc )


decl : Localizer -> String -> List String -> List ( String, List Type.Canonical ) -> Doc
decl localizer name vars tags =
    let
        docTag ( tag, args ) =
            hang 2 (sep (string tag :: List.map (docType localizer App) args))
    in
    hang 4 <|
        vcat <|
            hsep (string "type" :: List.map string (name :: vars))
                :: List.map2 withSpace
                    (string "=" :: repeat (string "|") tags)
                    (List.map docTag tags)


annotation : Localizer -> String -> Type.Canonical -> Doc
annotation localizer name tipe =
    let
        docName =
            if Help.isOp name then
                parens (string name)

            else
                string name
    in
    case Type.collectLambdas tipe of
        (_ :: _ :: _) as parts ->
            hang 4 <|
                sep <|
                    docName
                        :: List.map2 withSpace
                            (string ":" :: repeat (string "->") parts)
                            (List.map (docType localizer Func) parts)

        _ ->
            hang 4 <|
                sep <|
                    [ withSpace docName (string ":")
                    , docType localizer None tipe
                    ]



-- LOCALIZE TYPES


type alias Localizer =
    GenericDict.AnyDict (List String) Var.Canonical String


varToDoc : Localizer -> Var.Canonical -> Doc
varToDoc localizer var =
    let
        name =
            Var.toString var
    in
    if name == Help.zeroTuple then
        string "()"

    else
        string (maybe name identity (GenericDict.get var localizer))



-- DIFF BUILDER


type Diff a
    = Diff a a
    | Same a


mapDiff : (a -> b) -> Diff a -> Diff b
mapDiff func d =
    -- Instance Functor
    case d of
        Diff left right ->
            Diff (func left) (func right)

        Same both ->
            Same (func both)


applyDiff : Diff a -> Diff (a -> b) -> Diff b
applyDiff argument function =
    -- Instance Applicative
    case ( function, argument ) of
        ( Diff leftFunc rightFunc, Diff leftArg rightArg ) ->
            Diff (leftFunc leftArg) (rightFunc rightArg)

        ( Diff leftFunc rightFunc, Same arg ) ->
            Diff (leftFunc arg) (rightFunc arg)

        ( Same func, Diff leftArg rightArg ) ->
            Diff (func leftArg) (func rightArg)

        ( Same func, Same arg ) ->
            Same (func arg)


liftDiff : a -> Diff a
liftDiff =
    Same


sequenceDiff : List (Diff a) -> Diff (List a)
sequenceDiff =
    List.foldr
        (\nextDiff acc -> liftDiff (::) |> applyDiff nextDiff |> applyDiff acc)
        (liftDiff [])


partitionDiffs : Dict.Dict comparable (Diff a) -> ( List ( comparable, ( a, a ) ), List ( comparable, a ) )
partitionDiffs dict =
    let
        collect key value ( diffKeys, sameKeys ) =
            case value of
                Diff left right ->
                    ( ( key, ( left, right ) ) :: diffKeys
                    , sameKeys
                    )

                Same both ->
                    ( diffKeys
                    , ( key, both ) :: sameKeys
                    )
    in
    Dict.foldr collect ( [], [] ) dict



-- DOC DIFF


type Context
    = None
    | Func
    | App


diff : Localizer -> Context -> Type.Canonical -> Type.Canonical -> Diff Doc
diff localizer context leftType rightType =
    let
        go =
            diff localizer
    in
    case ( leftType, rightType ) of
        ( Type.Aliased leftName leftArgs _, Type.Aliased rightName rightArgs _ ) ->
            if leftName == rightName then
                mapDiff
                    (docAppHelp localizer context leftName)
                    (sequenceDiff (List.map2 (go App) (List.map Tuple.second leftArgs) (List.map Tuple.second rightArgs)))

            else
                difference
                    (docType localizer context leftType)
                    (docType localizer context rightType)

        ( Type.Lambda _ _, _ ) ->
            diffLambda localizer context leftType rightType

        ( _, Type.Lambda _ _ ) ->
            diffLambda localizer context leftType rightType

        ( Type.Var x, Type.Var y ) ->
            if x == y then
                liftDiff (string x)

            else
                difference
                    (docType localizer context leftType)
                    (docType localizer context rightType)

        ( Type.Type leftName leftArgs, Type.Type rightName rightArgs ) ->
            if leftName /= rightName || List.length leftArgs /= List.length rightArgs then
                difference
                    (docApp localizer context leftName leftArgs)
                    (docApp localizer context rightName rightArgs)

            else
                let
                    subContext =
                        if Var.isTuple leftName then
                            None

                        else
                            App
                in
                mapDiff
                    (docAppHelp localizer context leftName)
                    (sequenceDiff (List.map2 (go subContext) leftArgs rightArgs))

        ( Type.Record outerLeftFields outerLeftExt, Type.Record outerRightFields outerRightExt ) ->
            let
                ( leftFields, leftExt ) =
                    flattenRecord outerLeftFields outerLeftExt

                ( rightFields, rightExt ) =
                    flattenRecord outerRightFields outerRightExt
            in
            diffRecord localizer leftFields leftExt rightFields rightExt

        ( _, _ ) ->
            difference
                (docType localizer context leftType)
                (docType localizer context rightType)


difference : Doc -> Doc -> Diff Doc
difference leftDoc rightDoc =
    Diff (dullyellow leftDoc) (dullyellow rightDoc)



-- FUNCTION DIFFS


diffLambda : Localizer -> Context -> Type.Canonical -> Type.Canonical -> Diff Doc
diffLambda localizer context leftType rightType =
    let
        leftArgs =
            List.reverse <| Type.collectLambdas leftType

        rightArgs =
            List.reverse <| Type.collectLambdas rightType

        extraToDoc types =
            List.map (dullyellow << docType localizer Func) types

        extraLefts =
            List.reverse <| extraToDoc <| List.drop (List.length rightArgs) leftArgs

        extraRights =
            List.reverse <| extraToDoc <| List.drop (List.length leftArgs) rightArgs

        alignedArgDiff =
            mapDiff List.reverse <|
                sequenceDiff (List.map2 (diff localizer Func) leftArgs rightArgs)
    in
    mapDiff
        (docLambda context)
        (case ( extraLefts, extraRights, alignedArgDiff ) of
            ( [], [], _ ) ->
                alignedArgDiff

            ( _, _, Same docs ) ->
                Diff (extraLefts ++ docs) (extraRights ++ docs)

            ( _, _, Diff lefts rights ) ->
                Diff (extraLefts ++ lefts) (extraRights ++ rights)
        )



-- RECORD DIFFS


diffRecord : Localizer -> Fields -> Maybe String -> Fields -> Maybe String -> Diff Doc
diffRecord localizer leftFields leftExt rightFields rightExt =
    let
        ( leftOnly, both, rightOnly ) =
            vennDiagram leftFields rightFields
    in
    if Dict.isEmpty leftOnly && Dict.isEmpty rightOnly then
        let
            fieldDiffs =
                Dict.map (\_ -> \( a, b ) -> diff localizer None a b) both
        in
        case partitionDiffs fieldDiffs of
            ( [], sames ) ->
                let
                    mkRecord =
                        docRecord Full (List.map (mapBoth string identity) sames)
                in
                if leftExt == rightExt then
                    Same (mkRecord (Maybe.map string leftExt))

                else
                    Diff
                        (mkRecord (Maybe.map (dullyellow << string) leftExt))
                        (mkRecord (Maybe.map (dullyellow << string) rightExt))

            ( diffs, sames ) ->
                let
                    ( lefts, rights ) =
                        unzipDiffs diffs

                    style =
                        if List.isEmpty sames then
                            Full

                        else
                            Elide
                in
                Diff
                    (docRecord style lefts (Maybe.map string leftExt))
                    (docRecord style rights (Maybe.map string rightExt))

    else
        let
            ( lefts, rights ) =
                analyzeFields (Dict.keys leftOnly) (Dict.keys rightOnly)

            style =
                if Dict.isEmpty both then
                    Full

                else
                    Elide
        in
        Diff
            (docRecord style lefts (Maybe.map string leftExt))
            (docRecord style rights (Maybe.map string rightExt))


unzipDiffs : List ( String, ( Doc, Doc ) ) -> ( List ( Doc, Doc ), List ( Doc, Doc ) )
unzipDiffs diffPairs =
    let
        unzipHelp ( name, ( left, right ) ) =
            ( ( string name, left ), ( string name, right ) )
    in
    List.unzip (List.map unzipHelp diffPairs)



-- RECORD DIFFS HELPERS


analyzeFields : List String -> List String -> ( List ( Doc, Doc ), List ( Doc, Doc ) )
analyzeFields leftOnly rightOnly =
    let
        typoPairs =
            Help.findTypoPairs leftOnly rightOnly

        mkField func name =
            ( func (string name), string "..." )

        mkFieldWith counts name =
            mkField
                (if Set.member name counts then
                    dullyellow

                 else
                    identity
                )
                name
    in
    case Help.vetTypos typoPairs of
        Just ( leftTypos, rightTypos ) ->
            ( List.map (mkFieldWith leftTypos) leftOnly
            , List.map (mkFieldWith rightTypos) rightOnly
            )

        Nothing ->
            ( List.map (mkField dullyellow) leftOnly
            , List.map (mkField dullyellow) rightOnly
            )


type alias Fields =
    Dict.Dict String Type.Canonical


type alias DoubleFields =
    Dict.Dict String ( Type.Canonical, Type.Canonical )


vennDiagram : Fields -> Fields -> ( Fields, DoubleFields, Fields )
vennDiagram leftFields rightFields =
    ( Dict.diff leftFields rightFields
    , Dict.merge
        (\_ _ x -> x)
        (\key left right -> Dict.insert key ( left, right ))
        (\_ _ x -> x)
        leftFields
        rightFields
        Dict.empty
    , Dict.diff rightFields leftFields
    )


flattenRecord :
    List ( String, Type.Canonical )
    -> Maybe Type.Canonical
    -> ( Fields, Maybe String )
flattenRecord fields ext =
    mapBoth Dict.fromList identity (flattenRecordHelp fields ext)


flattenRecordHelp :
    List ( String, Type.Canonical )
    -> Maybe Type.Canonical
    -> ( List ( String, Type.Canonical ), Maybe String )
flattenRecordHelp fields ext =
    case ext of
        Nothing ->
            ( fields, Nothing )

        Just (Type.Var x) ->
            ( fields, Just x )

        Just (Type.Record subFields subExt) ->
            flattenRecordHelp (fields ++ subFields) subExt

        Just (Type.Aliased _ args tipe) ->
            flattenRecordHelp fields (Just (Type.dealias args tipe))

        _ ->
            Debug.todo "Trying to flatten ill-formed record."



-- TYPES TO DOCS


docType : Localizer -> Context -> Type.Canonical -> Doc
docType localizer context tipe =
    case tipe of
        Type.Lambda _ _ ->
            docLambda context (List.map (docType localizer Func) (Type.collectLambdas tipe))

        Type.Var x ->
            string x

        Type.Type name args ->
            docApp localizer context name args

        Type.Record outerFields outerExt ->
            let
                ( fields, ext ) =
                    flattenRecordHelp outerFields outerExt
            in
            docRecord Full
                (List.map (mapBoth string (docType localizer None)) fields)
                (Maybe.map string ext)

        Type.Aliased name args _ ->
            docApp localizer context name (List.map Tuple.second args)


docLambda : Context -> List Doc -> Doc
docLambda context docs =
    case docs of
        [] ->
            Debug.todo "cannot call docLambda with an empty list"

        arg :: rest ->
            case context of
                None ->
                    sep (arg :: List.map (withSpace (string "->")) rest)

                _ ->
                    cat
                        [ string "("
                        , sep (arg :: List.map (withSpace (string "->")) rest)
                        , string ")"
                        ]


docApp : Localizer -> Context -> Var.Canonical -> List Type.Canonical -> Doc
docApp localizer context name args =
    let
        argContext =
            if Var.isTuple name then
                None

            else
                App
    in
    docAppHelp localizer context name (List.map (docType localizer argContext) args)


docAppHelp : Localizer -> Context -> Var.Canonical -> List Doc -> Doc
docAppHelp localizer context name arguments =
    case arguments of
        [] ->
            if Var.isTuple name then
                string "()"

            else
                varToDoc localizer name

        arg :: args ->
            if Var.isTuple name then
                sep
                    [ cat
                        (withSpace (string "(") arg
                            :: List.map (withSpace (string ",")) args
                        )
                    , string ")"
                    ]

            else
                case context of
                    App ->
                        cat
                            [ string "("
                            , hang 4 (sep (varToDoc localizer name :: arg :: args))
                            , string ")"
                            ]

                    _ ->
                        hang 4 (sep (varToDoc localizer name :: arg :: args))


type Style
    = Elide
    | Full


docRecord : Style -> List ( Doc, Doc ) -> Maybe Doc -> Doc
docRecord style fields maybeExt =
    let
        docField ( name, tipe ) =
            hang 4 (sep [ withSpace name (string ":"), tipe ])

        elision =
            case style of
                Full ->
                    []

                Elide ->
                    [ string "..." ]

        fieldDocs =
            elision ++ List.map docField fields
    in
    case ( fieldDocs, maybeExt ) of
        ( [], Nothing ) ->
            string "{}"

        ( _, Nothing ) ->
            sep
                [ cat (List.map2 withSpace (string "{" :: repeat (string ",") fieldDocs) fieldDocs)
                , string "}"
                ]

        ( _, Just ext ) ->
            sep
                [ hang 4 <|
                    sep <|
                        [ withSpace (string "{") ext
                        , cat (List.map2 withSpace (string "|" :: repeat (string ",") fieldDocs) fieldDocs)
                        ]
                , string "}"
                ]
