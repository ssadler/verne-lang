module Verne.Data.Code
  ( Code(..)
  , Syntax(..)
  ) where

import Data.Array

import Verne.Data.Component

-- | Code Tree
data Code = Code Code (Array Code)
          | Atom Component
          | Posc Int Int Code

-- | Syntax Tree
data Syntax = Syntax Syntax (Array Syntax)
            | Name String
            | Str String
            | Posi Int Int Syntax
