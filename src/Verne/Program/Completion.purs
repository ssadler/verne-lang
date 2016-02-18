module Verne.Program.Completion
  ( CompletionResult
  , getCompletion
  ) where

import Control.Monad.Except.Trans

import Data.Array (last)
import Data.Either
import Data.Maybe
import Data.Traversable

import Verne.Types.Program

import Prelude

type Complete = ExceptT CompletionResult Program

data CompletionResult = Completion | NoCompletion

getCompletion :: Int -> Code -> Program CompletionResult
getCompletion caret code = do
  o <- runExceptT $ complete caret code
  case o of
       Left r -> pure r
       Right _ -> pure NoCompletion

complete :: Int -> Code -> Complete Unit
complete caret (List {pos,head,args}) =
  if caret < pos.a || caret > pos.b then pure unit else do
    complete caret head
    traverse (complete caret) args
    case getPos <$> last args of 
      Just {b} | caret > b + 1 -> completeNextArg caret head args
      _ -> pure unit
complete caret (Atom {pos,component}) = 
  if caret < pos.a || caret > pos.b then pure unit else do
    pure unit

completeNextArg :: Int -> Code -> Array Code -> Complete Unit
completeNextArg caret head args = pure unit

getPos :: Code -> Pos
getPos (Atom {pos}) = pos
getPos (List {pos}) = pos

