module Verne.Data.Program
  ( module SM
  , Namespace(..)
  , Program(..)
  , ProgramState(..)
  , addPart
  , getNameCompletions
  , newProgramState
  , runProgram
  ) where

import Control.Monad.State

import Data.Array (reverse, take)
import Data.List (List(..), filter)
import Data.Foreign
import Data.Maybe
import Data.String (length, take, fromCharArray) as S
import Data.StrMap (StrMap(..), foldMap, empty, insert, lookup)
import Data.StrMap (lookup) as SM
import Data.Tuple (Tuple(..))

import Verne.Data.Code
import Verne.Data.Part
import Verne.Data.Type
import Verne.Data.Hashable

import Prelude


type Namespace = StrMap Part


type Program = State ProgramState


-- | Programstate
newtype ProgramState =
  Ps { globals :: Namespace -- Todo: get rid of globals. All names live under a module.
     , modules :: StrMap Namespace
     }


-- | Run a Program computation
runProgram :: forall a. ProgramState -> Program a -> Tuple a ProgramState
runProgram ps act = runState act ps


-- | Empty program state
newProgramState :: ProgramState
newProgramState = Ps { globals: empty
                     , modules: empty
                     }


-- Add a part to program
addPart :: Part -> Program Unit
addPart c@(Part {name}) = modify (\(Ps s@{globals}) ->
    Ps $ s { globals = insert name c globals })


-- | Get parts with name matching prefix
getNameCompletions :: String -> Type -> Program (Array Part)
getNameCompletions prefix typ =
  getMatchingParts (\name (Part part) ->
                prefix == S.take (S.length prefix) name
                && typeEndsWith part."type" typ)


-- | Get parts with qualifying name
getMatchingParts :: (String -> Part -> Boolean)
            -> Program (Array Part)
getMatchingParts match = 
  let go a b = if match a b then [b] else []
   in gets (\(Ps {globals}) -> foldMap go globals)
