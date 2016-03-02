module Verne.Types
  ( module SC
  , Error(..)
  , Program(..)
  , ProgramState(..)
  ) where

import Control.Monad.State
import Control.Monad.State.Class (get, modify) as SC
import Data.List (List(..))
import Data.StrMap

import Verne.Data.Code
import Verne.Data.Namespace

-- | Core language types
--
type Error = String

-- | Program monad
-- TODO: Define this thing better. Where does the pointer to the current module live?
--       Is Program the execution context?
--
type Program = State ProgramState


-- | Programstate
--
newtype ProgramState =
  Ps { globals :: Namespace -- Todo: get rid of globals. All names live under a module.
     , modules :: StrMap Namespace
     }

