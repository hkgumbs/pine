module AST.Kernel exposing
    ( Chunk(..)
    , Info(..)
    )

import AST.Module.Name as ModuleName



-- INFO


type Info
    = Info (List ( ModuleName.Raw, String )) (List Chunk)


type Chunk
    = JS String
    | Var ModuleName.Raw String
    | Field String
    | Prod Bool
