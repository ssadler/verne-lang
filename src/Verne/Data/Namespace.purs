module Verne.Data.Namespace 
  ( module SM
  , Namespace(..)
  , lookupName
  , nameObject
  , getNameCompletions
  ) where

import Data.Array (reverse, take)
import Data.List (List(..), filter, fromList)
import Data.Foreign
import Data.Maybe
import qualified Data.String (length, take, fromCharArray) as S
import Data.StrMap (StrMap(..), lookup, toList)
import Data.StrMap (empty, insert, lookup) as SM
import Data.Tuple (Tuple(..))

import Verne.Data.Code
import Verne.Data.Object
import Verne.Types.Hashable

import Prelude
type Namespace = StrMap Object

getNameCompletions :: Array String -> String -> Namespace
                   -> Array (Tuple String Object)
getNameCompletions typ pref ns = fromList $ filter go $ toList ns
  where
  go (Tuple name (Object com)) =
    (pref == S.take (S.length pref) name) &&
    typ == lastType com."type"
