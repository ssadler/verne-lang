module Verne.Program
  ( program
  ) where

import Control.Monad.State

import Data.Either
import Data.Foreign
import Data.Foreign.Class

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

addPart :: Foreign -> Program (Either ForeignError Unit)
addPart fo =
  case read fo of
       Right com -> Right <$> mod com
       Left fe   -> pure (Left fe)
  where
  mod c@(Part {name}) = modify (\(Ps s@{globals}) ->
    Ps $ s { globals = insert name c globals })

program :: Foreign
program = make { newProgramState
               , addPart
               , parse
               , compile
               , toExecutable
               , execute
               , runState
               , showCodeError
               , codeErrors
               , getCompletion
               , getNameCompletions
               }

foreign import make :: forall a. {| a} -> Foreign
