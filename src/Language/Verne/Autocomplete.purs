module Autocomplete where

import Language.Verne.Namespace
import Language.Verne.TypeChecker
import Language.Verne.Types

import Prelude

-- | Strategy: Find the thing where the caret is.
getCompletions :: LISP_T Atom -> Int -> Unit
getCompletions (LIST_T {pos=Pos from to, arr=arr}) caret = unit
