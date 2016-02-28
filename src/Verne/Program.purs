module Verne.Program
  ( program
  ) where

import Control.Monad.State

import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Data.Monoid
import Data.Tuple

import Prelude

import Verne.Data.Code
import Verne.Data.Object
import Verne.Data.Namespace
import Verne.Program.Compiler
import Verne.Program.Parser
import Verne.Types.Program

newProgramState :: ProgramState
newProgramState = Ps { parsers: mempty
                     , globals: empty
                     , modules: empty
                     }

addObject :: Foreign -> Program (Either ForeignError Unit)
addObject fo =
  case read fo of
       Right com -> Right <$> mod com
       Left fe   -> pure (Left fe)
  where
  mod c@(Object {name}) = modify (\(Ps s@{globals}) ->
    Ps $ s { globals = insert name c globals })

program :: Foreign
program = make { newProgramState
               , runState
               , parse
               , compile
               , addObject
               }

foreign import make :: forall a. {| a} -> Foreign
