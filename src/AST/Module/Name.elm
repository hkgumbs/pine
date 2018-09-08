module AST.Module.Name exposing
    ( Canonical(..)
    , Raw
    , canonicalIsKernel
    , canonicalToString
    , getKernel
    , inCore
    , inHtml
    , inVirtualDom
    , isKernel
    )

import Elm.Package as Package



-- NAMES


type alias Raw =
    String



-- must be non-empty


type Canonical
    = Canonical Package.Name Raw



-- HELPERS


inVirtualDom : Raw -> Canonical
inVirtualDom raw =
    Canonical Package.virtualDom raw


inCore : Raw -> Canonical
inCore raw =
    Canonical Package.core raw


inHtml : Raw -> Canonical
inHtml raw =
    Canonical Package.html raw



-- CONVERSIONS


canonicalToString : Canonical -> String
canonicalToString (Canonical _ name) =
    name



-- IS KERNEL


isKernel : Raw -> Bool
isKernel name =
    String.startsWith "Elm.Kernel." name


getKernel : Raw -> String
getKernel name =
    String.dropLeft 11 name


canonicalIsKernel : Canonical -> Bool
canonicalIsKernel (Canonical _ name) =
    isKernel name
