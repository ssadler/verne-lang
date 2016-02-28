module Verne.Data.Code
  ( Code(..)
  , Syntax(..)
  ) where

import Data.Array

import Verne.Data.Part

-- | Code Tree
data Code = Code Code (Array Code)
          | Obj Part
          | Posc Int Int Code

-- | Syntax Tree
data Syntax = Syntax Syntax (Array Syntax)
            | Name String
            | Str String
            | Posi Int Int Syntax
