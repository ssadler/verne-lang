module Language.Verne.Runner
    ( prepare
    ) where

import Data.Either
import Data.Foreign
import Data.Maybe
import Data.Traversable

import Language.Verne.Types
import Language.Verne.TypeChecker

import Prelude

type ErrorPos = {error :: Error, pos :: Pos}

prepare :: LISP_T Atom -> Either ErrorPos Foreign
prepare (LIST_T {pos=pos, merr=(Just error)}) = Left {error, pos}
prepare (LIST_T {merr=Nothing, arr}) =
    case sequence (prepare <$> arr) of
        Right comps -> Right (toForeign {expr: comps})
        Left errpos -> Left errpos
prepare (ATOM_T {pos=pos, ecomp=(Left error)}) = Left {error, pos}
prepare (ATOM_T {ecomp=(Right component)}) = Right (toForeign {atom: component})

