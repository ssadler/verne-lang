module Language.Verne.Runner
    ( prepare
    ) where

import Data.Either
import Data.Foreign
import Data.Maybe
import Data.Traversable

import Language.Verne.Types

import Prelude

type ErrorPos = {error :: Error, pos :: Pos}

prepare :: LISP_T Atom -> Either ErrorPos Foreign
prepare (LIST_T _ pos (Just error) _) = Left {error, pos}
prepare (LIST_T _ _   Nothing arr) =
    case sequence (prepare <$> arr) of
        Right comps -> Right (toForeign {expr: comps})
        Left errpos -> Left errpos
prepare (ATOM_T _ pos _ (Left error)) = Left {error, pos}
prepare (ATOM_T _ _   _ (Right component)) = Right (toForeign {atom: component})

