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
import Verne.Data.Part
import Verne.Data.Namespace
import Verne.Program.Compiler
import Verne.Program.Parser
import Verne.Types.Program

newProgramState :: ProgramState
newProgramState = Ps { parsers: mempty
                     , globals: empty
                     , modules: empty
                     }

addPart :: Foreign -> Program (Either ForeignError Part)
addPart fo =
  case read fo of
       Right com -> Right <$> mod com
       Left fe   -> pure (Left fe)
  where
  mod c@(Part {name}) = modify (\(Ps s@{globals}) ->
    Ps $ s { globals = insert name c globals })

program :: Foreign
program = make { newProgramState
               , runState
               , parse
               , compile
               , addPart
               }

foreign import make :: forall a. {| a} -> Foreign
