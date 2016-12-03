module Verne.Program
  ( module Verne.Data.Program
  , addPart
  , newProgramState
  ) where

import Control.Monad.State

import Data.Either

import Prelude

import Verne.Data.Code
import Verne.Data.Part
import Verne.Data.Program

import Verne.Compiler
import Verne.Exec
import Verne.Parser

newProgramState :: ProgramState
newProgramState = Ps { globals: empty
                     , modules: empty
                     }

addPart :: Part -> Program Unit
addPart c@(Part {name}) = modify (\(Ps s@{globals}) ->
    Ps $ s { globals = insert name c globals })
