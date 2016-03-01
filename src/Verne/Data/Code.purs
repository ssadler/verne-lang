module Verne.Data.Code
  ( Code(..)
  , Syntax(..)
  ) where

import Data.Array

import Verne.Data.Type
import Verne.Data.Part

-- | IR Concept
-- 
-- Having many fixed intermediary representations is a burden
-- because the cost of experimentation is high and the scope
-- of each transformation is unclear.
-- 
-- Instead, have a single IR with constructors to support or
-- annotate every case.
-- 
-- Modules that implement transformations wrap their outputs
-- in types that specify which IR constructors may be
-- present.
--
-- TODO: Draw diagrams of these transitions and what the resulting
-- configurations look like, ie, what can contain what.
--
-- This style is flexible but it is working around the type system
-- to an extent, because much of the information about a value is
-- encoded in an opaque type tree. Would be interesting to see if
-- there's a way around this that does not involve type classes.

-- | Code Tree
-- | This could actually be merged with the Syntax Tree.
data Code = Code Code (Array Code)
          | Atom Part
          | Undefined String Type
          | NeedsArgument Code
          | TooManyArguments (Array Syntax) Code
          | TypeError Type Type
          | Posc Int Int Code

-- | Syntax Tree
data Syntax = Syntax Syntax (Array Syntax)
            | Name String
            | Str String
            | Posi Int Int Syntax
