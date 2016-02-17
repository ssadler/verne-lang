module Verne.Program.Compiler
  ( compile
  ) where

import Control.Monad (when)
import Control.Monad.Except.Trans

import Data.Array (foldM, drop, take)
import Data.Either
import Data.Foreign
import Data.Maybe
import Data.Traversable

import Prelude

import Verne.Types
import Verne.Utils

type Compile = ExceptT Error Program

compile :: Code -> Program (Either Error Component)
compile = runExceptT <<< compile'

compile' :: Code -> Compile Component
compile' (Atom {component=c}) = pure c
compile' (List {head,args}) = do
  ch <- compile' head
  ct <- traverse compile' args
  foldM curry ch ct


curry :: Component -> Component -> Compile Component
curry (Component c1) (Component c2) = Component <$> do
  when (take 1 c1.signature /= c2.signature)
       (ExceptT (pure (Left "curry mismatch")))
  pure { id: hashMany [c1.id, c2.id]
       , name: ""
       , signature: drop 1 c1.signature
       , exec: curryForeign c1.exec c2.exec
       , autocomplete: Nothing
       }

