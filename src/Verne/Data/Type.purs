module Verne.Data.Type where

import qualified Data.Array.Unsafe (last) as U

data Type = List  | Atom String | Unit

last :: Type -> Type
last (List l) = last l
last t = t
