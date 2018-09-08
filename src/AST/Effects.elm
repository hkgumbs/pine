module AST.Effects exposing
    ( Canonical
    , Effects(..)
    , Info(..)
    , Kind(..)
    , ManagerType(..)
    , PortCanonical(..)
    , PortRaw(..)
    , Raw
    , RawManagerType(..)
    )

import AST.Type as Type
import Elm.Package as Pkg
import Reporting.Annotation as A
import Reporting.Region as R



-- EFFECTS


type Effects pkg ports
    = None
    | Manager pkg Info
    | Port ports


type alias Raw =
    Effects () (List (A.Commented PortRaw))


type alias Canonical =
    Effects Pkg.Name (List (A.Commented PortCanonical))



-- EFFECT MANAGERS


type Info
    = Info
        { tag : R.Region
        , init : R.Region
        , onEffects : R.Region
        , onSelfMsg : R.Region
        , managerType : RawManagerType
        }


type RawManagerType
    = CmdManager (A.Located String)
    | SubManager (A.Located String)
    | FxManager (A.Located String) (A.Located String)


type ManagerType
    = Cmds
    | Subs
    | Both



-- FOREIGN EFFECTS


type PortRaw
    = PortRaw String Type.Raw


type PortCanonical
    = PortCanonical String Kind Type.Canonical


type Kind
    = Outgoing Type.Canonical
    | Incoming Type.Canonical
