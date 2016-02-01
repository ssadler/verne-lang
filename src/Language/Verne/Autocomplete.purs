module Autocomplete where

import Language.Verne.Namespace
import Language.Verne.Types

data LISP_T a = LIST_T Type Pos (Maybe Error) (Array (LISP_T a))
              | ATOM_T Type Pos a (Either Error Component)

getCompletions :: LISP_T Atom -> Int -> Unit
getCompletions (LIST_T _ _ _ arr) caret =
