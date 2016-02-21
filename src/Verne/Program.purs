module Verne.Program
  ( program
  ) where

import Control.Monad.State

import Data.Either
import Data.Foreign
import Data.Maybe
import Data.Monoid
import Data.Tuple

import Prelude

import Verne.Data.Code
import Verne.Data.Component
import Verne.Data.Namespace
import Verne.Program.Compiler
import Verne.Program.Parser
import Verne.Types.Program

newProgramState :: ProgramState
newProgramState = PS { parsers: mempty
                     , globals: empty
                     , modules: empty
                     }

addComponent :: Component -> ProgramState -> ProgramState
addComponent c@(Component {name}) (PS (s@{globals})) =
  PS (s { globals = insert name c globals })

program :: Foreign
program = make { newProgramState
               , runState
               , parse 
               , compile 
               , addComponent 
               , maybe
               , either 
               }

foreign import make :: forall a. {| a} -> Foreign
