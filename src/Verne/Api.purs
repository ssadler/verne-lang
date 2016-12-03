module Verne.Api (
  module Data.Either
  , program
  ) where

import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Prelude

import Verne.Compiler
import Verne.Data.Code
import Verne.Data.Program
import Verne.Exec
import Verne.Parser
import Verne.Program


importPart :: Foreign -> Program (Either ForeignError Unit)
importPart fo =
  case read fo of
       Right com -> Right <$> addPart com
       Left fe   -> pure (Left fe)


program :: Foreign
program = make { newProgramState
               , importPart
               , parse
               , compile
               , toExecutable
               , execute
               , showCodeError
               , codeErrors
               , getCodeAtPosition
               , getNameCompletions
               }

foreign import make :: forall a. {| a} -> Foreign
