module Verne.Data.Program
  ( module SM
  , Namespace(..)
  , Program(..)
  , ProgramState(..)
  , getNameCompletions
  , searchNames
  ) where

import Control.Monad.State

import Data.Array (reverse, take)
import Data.List (List(..), filter, fromList)
import Data.Foreign
import Data.Maybe
import Data.String (length, take, fromCharArray) as S
import Data.StrMap (StrMap(..), foldMap)
import Data.StrMap (empty, insert, lookup) as SM
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

type NamedPart = {name::String, part::Part}

getNameCompletions :: String -> Type -> Program (Array NamedPart)
getNameCompletions pref typ =
  searchNames (\name (Part part) ->
                pref == S.take (S.length pref) name
                && typeEndsWith part."type" typ)

searchNames :: (String -> Part -> Boolean)
            -> Program (Array NamedPart)
searchNames match = 
  let go a b = if match a b then [{name:a,part:b}] else []
   in gets (\(Ps {globals}) -> foldMap go globals)
