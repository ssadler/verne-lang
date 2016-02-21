module Verne.Data.Namespace 
  ( module SM
  , Namespace(..)
  , lookupName
  , nameComponent
  , nameCompletion
  ) where

import Data.List (List(..), fromList)
import Data.Foreign
import Data.Maybe
import Data.String (fromCharArray)
import Data.StrMap (StrMap(..), lookup)
import Data.StrMap (empty, insert) as SM

import Verne.Data.Code
import Verne.Data.Component
import Verne.Types.Hashable

import Prelude
type Namespace = StrMap Component

lookupName :: String -> Namespace -> Maybe Component
lookupName = lookup

nameComponent :: String -> Component
nameComponent name =
  Component { id: hash ["Name Component", name]
            , name: name
            , signature: ["_lookup"]
            , exec: toForeign (resolveName name)
            , autocomplete: Nothing
            }

foreign import nameCompletion :: forall a b. a -> b

resolveName :: String -> Namespace -> Component
resolveName name ns = 
  case lookupName name ns of
       Just c -> c
       Nothing -> cantResolve name

cantResolve :: String -> Component
cantResolve name =
  Component { id : hash ["Can't resolve", name]
            , name: ""
            , signature: [""]
            , exec: toForeign (\a -> a)
            , autocomplete: Just (toForeign nameCompletion)
            }
