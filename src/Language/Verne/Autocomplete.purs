module Autocomplete where

import Language.Verne.Namespace
import Language.Verne.TypeChecker
import Language.Verne.Types

import Prelude

getCompletions :: LISP_T Atom -> Int -> Unit
getCompletions (LIST_T {arr=arr}) caret = unit
