module Verne.Data.Namespace 
  ( module SM
  , Namespace(..)
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
import Verne.Data.Part
import Verne.Data.Type
import Verne.Data.Hashable

import Prelude
type Namespace = StrMap Part


getNameCompletions :: Type -> String -> Namespace
                   -> Array (Tuple String Part)
getNameCompletions typ pref ns = fromList $ filter go $ toList ns
  where
  go (Tuple name (Part com)) =
    (pref == S.take (S.length pref) name) &&
    typ == lastType com."type"
