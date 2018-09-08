module Optimize.DecisionTree exposing (..)

{- To learn more about how this works, definitely read through:

       "When Do Match-Compilation Heuristics Matter?"

   by Kevin Scott and Norman Ramsey. The rough idea is that we start with a simple
   list of patterns and expressions, and then turn that into a "decision tree"
   that requires as few tests as possible to make it to a leaf. Read the paper, it
   explains this extraordinarily well! We are currently using the same heuristics
   as SML/NJ to get nice trees.
-}

import AST.Helpers as Help
import AST.Literal as L
import AST.Pattern as P
import AST.Variable as Var
import Dict exposing (Dict)
import GenericDict exposing (GenericDict)
import Prelude exposing (lookup)
import Reporting.Annotation as A


type alias CPattern =
    P.Canonical



-- COMPILE CASES


{-| Users of this module will mainly interact with this function. It takes
some normal branches and gives out a decision tree that has "labels" at all
the leafs and a dictionary that maps these "labels" to the code that should
run.

If 2 or more leaves point to the same label, we need to do some tricks in JS to
make that work nicely. When is JS getting goto?! ;) That is outside the scope
of this module though.

-}
compile : VariantDict -> List ( CPattern, Int ) -> DecisionTree
compile variantDict rawBranches =
    let
        format ( pattern, index ) =
            Branch index [ ( Empty, pattern ) ]
    in
    toDecisionTree variantDict (List.map format rawBranches)


{-| When a certain union type is defined, you specify a certain number of tags.
This helps us do a few optimizations:

  - If there is only one possible tag, we can always skip checking what it is.
    Tuples are a common example of this.
  - If we use all possible tags, we can skip doing the last test. If we have
    checked N-1 of N tags, there is no need to test the Nth, we know its the
    one we want

So this dictionary maps tags to the number of variants that exist, so it'll
contain things like [ ("Just", 2), ("Nothing", 2), ("#2", 1), ... ] which
we can use for these optimizations.

-}
type alias VariantDict =
    GenericDict Var.Home (Dict String Int)



-- DECISION TREES


type DecisionTree
    = Match Int
    | Decision Path (List ( Test, DecisionTree )) (Maybe DecisionTree)


type Test
    = Constructor Var.Canonical
    | Literal L.Literal


type Path
    = Position Int Path
    | Field String Path
    | Empty
    | Alias



-- PATH HELPERS


add : Path -> Path -> Path
add path finalLink =
    case path of
        Empty ->
            finalLink

        Alias ->
            Debug.todo "nothing should be added to an alias path"

        Position index subpath ->
            Position index (add subpath finalLink)

        Field name subpath ->
            Field name (add subpath finalLink)


subPositions : Path -> List CPattern -> List ( Path, CPattern )
subPositions path patterns =
    List.indexedMap
        (\index pattern -> ( add path (Position index Empty), pattern ))
        patterns



-- ACTUALLY BUILD DECISION TREES


type Branch
    = Branch Int (List ( Path, CPattern ))


toDecisionTree : VariantDict -> List Branch -> DecisionTree
toDecisionTree variantDict rawBranches =
    let
        branches =
            List.map (flattenPatterns variantDict) rawBranches
    in
    case checkForMatch branches of
        Just goal ->
            Match goal

        Nothing ->
            let
                path =
                    pickPath variantDict branches

                ( edges, fallback ) =
                    gatherEdges variantDict branches path

                decisionEdges =
                    List.map (Tuple.mapSecond (toDecisionTree variantDict)) edges
            in
            case ( decisionEdges, fallback ) of
                ( [ ( _, decisionTree ) ], [] ) ->
                    decisionTree

                ( _, [] ) ->
                    Decision path decisionEdges Nothing

                ( [], _ :: _ ) ->
                    toDecisionTree variantDict fallback

                ( _, _ ) ->
                    Decision path decisionEdges (Just (toDecisionTree variantDict fallback))


isComplete : VariantDict -> List Test -> Bool
isComplete variantDict tests =
    case List.head tests of
        Just (Constructor var) ->
            getArity variantDict var == List.length tests

        Just (Literal (L.Boolean _)) ->
            List.length tests == 2

        _ ->
            False


getArity : VariantDict -> Var.Canonical -> Int
getArity variantDict (Var.Canonical home name) =
    case Maybe.andThen (Dict.get name) (GenericDict.get home variantDict) of
        Just arity ->
            arity

        Nothing ->
            if Help.isTuple name then
                case String.toInt (String.dropLeft 1 name) of
                    Ok n ->
                        n

                    Err _ ->
                        Debug.todo <|
                            "`"
                                ++ name
                                ++ "` is a tuple according to AST.Helpers"
                                ++ " but not a according to Optimize.DecisionTree"

            else
                Debug.todo <|
                    "Since the Optimize phase happens after canonicalization and type"
                        ++ " inference, it is impossible that a pattern cannot be found."



-- FLATTEN PATTERNS


{-| Flatten type aliases and use the VariantDict to figure out when a tag is
the only variant so we can skip doing any tests on it.
-}
flattenPatterns : VariantDict -> Branch -> Branch
flattenPatterns variantDict (Branch goal pathPatterns) =
    Branch goal (List.concatMap (flatten variantDict) pathPatterns)


flatten : VariantDict -> ( Path, CPattern ) -> List ( Path, CPattern )
flatten variantDict (( path, A.A ann pattern ) as pathPattern) =
    case pattern of
        P.Var _ ->
            [ pathPattern ]

        P.Anything ->
            [ pathPattern ]

        P.Alias alias_ realPattern ->
            ( add path Alias, A.A ann (P.Var alias_) )
                :: flatten variantDict ( path, realPattern )

        P.Record _ ->
            [ pathPattern ]

        P.Ctor tag patterns ->
            if getArity variantDict tag == 1 then
                List.concatMap (flatten variantDict) (subPositions path patterns)

            else
                [ pathPattern ]

        P.Literal _ ->
            [ pathPattern ]



-- SUCCESSFULLY MATCH


{-| If the first branch has no more "decision points" we can finally take that
path. If that is the case we give the resulting label and a mapping from free
variables to "how to get their value". So a pattern like (Just (x,\_)) will give
us something like ("x" => value.0.0)
-}
checkForMatch : List Branch -> Maybe Int
checkForMatch branches =
    case branches of
        (Branch goal patterns) :: _ ->
            if List.all (not << needsTests << Tuple.second) patterns then
                Just goal

            else
                Nothing

        _ ->
            Nothing



-- GATHER OUTGOING EDGES


gatherEdges : VariantDict -> List Branch -> Path -> ( List ( Test, List Branch ), List Branch )
gatherEdges variantDict branches path =
    let
        relevantTests =
            testsAtPath path branches

        allEdges =
            List.map (edgesFor path branches) relevantTests

        fallbacks =
            if isComplete variantDict relevantTests then
                []

            else
                List.filter (isIrrelevantTo path) branches
    in
    ( allEdges, fallbacks )



-- FIND RELEVANT TESTS


testsAtPath : Path -> List Branch -> List Test
testsAtPath selectedPath branches =
    let
        allTests =
            List.filterMap (testAtPath selectedPath) branches

        skipVisited test (( uniqueTests, visitedTests ) as curr) =
            if Maybe.withDefault False <| lookup test visitedTests then
                curr

            else
                ( test :: uniqueTests
                , ( test, True ) :: visitedTests
                )
    in
    Tuple.first (List.foldr skipVisited ( [], [] ) allTests)


testAtPath : Path -> Branch -> Maybe Test
testAtPath selectedPath (Branch _ pathPatterns) =
    case lookup selectedPath pathPatterns of
        Nothing ->
            Nothing

        Just (A.A _ pattern) ->
            case pattern of
                P.Ctor name _ ->
                    Just (Constructor name)

                P.Literal lit ->
                    Just (Literal lit)

                P.Var _ ->
                    Nothing

                P.Alias _ _ ->
                    Debug.todo "aliases should never reach 'testAtPath' function"

                P.Anything ->
                    Nothing

                P.Record _ ->
                    Nothing



-- BUILD EDGES


edgesFor : Path -> List Branch -> Test -> ( Test, List Branch )
edgesFor path branches test =
    ( test
    , List.filterMap (toRelevantBranch test path) branches
    )


toRelevantBranch : Test -> Path -> Branch -> Maybe Branch
toRelevantBranch test path ((Branch goal pathPatterns) as branch) =
    case extract path pathPatterns of
        Just ( start, A.A _ pattern, end ) ->
            case pattern of
                P.Ctor name patterns ->
                    if test == Constructor name then
                        Just (Branch goal (start ++ subPositions path patterns ++ end))

                    else
                        Nothing

                P.Literal lit ->
                    if test == Literal lit then
                        Just (Branch goal (start ++ end))

                    else
                        Nothing

                _ ->
                    Just branch

        _ ->
            Just branch


extract :
    Path
    -> List ( Path, CPattern )
    -> Maybe ( List ( Path, CPattern ), CPattern, List ( Path, CPattern ) )
extract selectedPath pathPatterns =
    case pathPatterns of
        [] ->
            Nothing

        (( path, pattern ) as first) :: rest ->
            if path == selectedPath then
                Just ( [], pattern, rest )

            else
                case extract selectedPath rest of
                    Nothing ->
                        Nothing

                    Just ( start, foundPattern, end ) ->
                        Just ( first :: start, foundPattern, end )



-- FIND IRRELEVANT BRANCHES


isIrrelevantTo : Path -> Branch -> Bool
isIrrelevantTo selectedPath (Branch _ pathPatterns) =
    case lookup selectedPath pathPatterns of
        Nothing ->
            True

        Just pattern ->
            not (needsTests pattern)


needsTests : CPattern -> Bool
needsTests (A.A _ pattern) =
    case pattern of
        P.Var _ ->
            False

        P.Anything ->
            False

        P.Alias _ _ ->
            Debug.todo "aliases should never reach 'isIrrelevantTo' function"

        P.Record _ ->
            False

        P.Ctor _ _ ->
            True

        P.Literal _ ->
            True



-- PICK A PATH


pickPath : VariantDict -> List Branch -> Path
pickPath variantDict branches =
    let
        allPaths =
            List.filterMap isChoicePath (List.concatMap (\(Branch _ patterns) -> patterns) branches)
    in
    case bests (addWeights (smallDefaults branches) allPaths) of
        [ path ] ->
            path

        tiedPaths ->
            case bests (addWeights (smallBranchingFactor variantDict branches) tiedPaths) of
                [] ->
                    Debug.todo "Impossible -- there is no best path"

                first :: _ ->
                    first


isChoicePath : ( Path, CPattern ) -> Maybe Path
isChoicePath ( path, pattern ) =
    if needsTests pattern then
        Just path

    else
        Nothing


addWeights : (Path -> Int) -> List Path -> List ( Path, Int )
addWeights toWeight paths =
    List.map (\path -> ( path, toWeight path )) paths


bests : List ( Path, Int ) -> List Path
bests allPaths =
    case allPaths of
        [] ->
            Debug.todo "Cannot choose the best of zero paths. This should never happen."

        ( headPath, headWeight ) :: weightedPaths ->
            let
                gatherMinimum ( path, weight ) (( minWeight, paths ) as acc) =
                    if weight == minWeight then
                        ( minWeight, path :: paths )

                    else if weight < minWeight then
                        ( weight, [ path ] )

                    else
                        acc
            in
            Tuple.second (List.foldl gatherMinimum ( headWeight, [ headPath ] ) weightedPaths)



-- PATH PICKING HEURISTICS


smallDefaults : List Branch -> Path -> Int
smallDefaults branches path =
    List.length (List.filter (isIrrelevantTo path) branches)


smallBranchingFactor : VariantDict -> List Branch -> Path -> Int
smallBranchingFactor variantDict branches path =
    let
        ( edges, fallback ) =
            gatherEdges variantDict branches path
    in
    List.length edges
        + (if List.isEmpty fallback then
            0

           else
            1
          )
