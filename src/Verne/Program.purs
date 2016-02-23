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
newProgramState = Ps { parsers: mempty
                     , globals: empty
                     , modules: empty
                     }

addComponent :: Component -> Program Unit
addComponent c@(Component {name}) = do
  modify $ \(Ps s@{globals}) ->
    Ps $ s { globals = insert name c globals }

program :: Foreign
program = make { newProgramState
               , runState
               , parse 
               , compile 
               , addComponent 
               }

foreign import make :: forall a. {| a} -> Foreign
