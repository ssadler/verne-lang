module Verne.Types.Program
  ( module SC
  , Error(..)
  , Program(..)
  , ProgramState(..)
  , Type(..)
  ) where

import Control.Monad.State
import Control.Monad.State.Class (get, modify) as SC
import Data.List (List(..))

import Text.Parsing.StringParser (Parser(..))

import Verne.Data.Code
import Verne.Data.Namespace

-- | Core language types
--
type Type = String

type Error = String

-- | Program monad
--
type Program = State ProgramState

newtype ProgramState = Ps { parsers :: List (Parser Code)
                          , globals :: Namespace
                          , modules :: Namespace
                          }

