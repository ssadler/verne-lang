module Verne.Program.Completion
  ( getCompletion
  ) where

import Control.Monad.Except.Trans

import Data.Array (last)
import Data.Either
import Data.Maybe
import Data.Traversable

import Verne.Data.Code
import Verne.Data.Component
import Verne.Types.Program
import Verne.Program.Compiler

import Prelude

type Complete a = ExceptT Component Program a

getCompletion :: Int -> Code -> Program (Maybe Component)
getCompletion caret code =
  either Just (\_ -> Nothing) <$> runExceptT (complete caret code)

complete :: Int -> Code -> Complete Unit
complete caret code@(List {pos,head,args}) =
  if caret < pos.a || caret > pos.b then pure unit else do
    case getPos <$> last args of 
      Just {b} | caret > b + 1 -> completeCompile code
      _ -> pure unit
    complete caret head
    traverse (complete caret) args
    pure unit
complete caret code@(Atom {pos,component}) = 
  if caret < pos.a || caret > pos.b then pure unit else
    case component of
      Component {autocomplete=Just _} -> completeCompile code
      _ -> pure unit

completeCompile :: Code -> Complete Unit
completeCompile code = do
  ecompiled <- lift (compile code)
  case ecompiled of
    Right c@(Component {autocomplete=Just _}) -> ExceptT (pure (Left c))
    _ -> pure unit

getPos :: Code -> Pos
getPos (Atom {pos}) = pos
getPos (List {pos}) = pos

