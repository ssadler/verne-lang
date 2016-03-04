module Verne.Exec where

import Control.Monad.Except.Trans

import Data.Either
import Data.Foreign
import Data.Function (Fn2, runFn2)
import Data.Traversable

import Prelude

import Verne.Data.Code
import Verne.Data.Part
import Verne.Data.Program

type Execute = ExceptT Foreign Program

execute :: Executable -> Program (Either Foreign Foreign)
execute (Executable code) = runExceptT (go code)
  where
  go :: Code -> Execute Foreign
  go (Atom part) = ExceptT $ pure $ runFn2 runPart part []
  go (Code (Posc _ _ (Atom func)) args) = do
    args <- traverse go args
    ExceptT $ pure $ runFn2 runPart func args
  go (Posc a b code) = go code

foreign import runPart :: Fn2 Part (Array Foreign) (Either Foreign Foreign)
