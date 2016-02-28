module Verne.Data.Namespace 
  ( module SM
  , Namespace(..)
  , lookupName
  , nameComponent
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
import Verne.Data.Component
import Verne.Types.Hashable

import Prelude
type Namespace = StrMap Component

nameComponent :: String -> Component
nameComponent name =
  Component { id: hash ["Name Component", name]
            , name: name
            , signature: ["_lookup"]
            , exec: toForeign "none"
            , autocomplete: Nothing
            }

getNameCompletions :: Array String -> String -> Namespace
                   -> Array (Tuple String Component)
getNameCompletions typ pref ns = fromList $ filter go $ toList ns
  where
  go (Tuple name (Component com)) =
    (pref == S.take (S.length pref) name) &&
    typ == (take 1 $ reverse com.signature)
