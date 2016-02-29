module Verne.Types.Program
  ( module SC
  , Error(..)
  , Program(..)
  , ProgramState(..)
  ) where

import Control.Monad.State
import Control.Monad.State.Class (get, modify) as SC
import Data.List (List(..))

import Verne.Data.Code
import Verne.Data.Namespace

-- | Core language types
--
type Error = String

-- | Program monad
--
type Program = State ProgramState

newtype ProgramState = Ps { globals :: Namespace
                          , modules :: Namespace
                          }

