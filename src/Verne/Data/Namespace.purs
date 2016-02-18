module Verne.Data.Namespace 
  ( Namespace(..)
  , lookupName
  , nameComponent
  ) where

import Data.Foreign
import Data.Maybe
import Data.StrMap (StrMap(..), lookup)
import Verne.Types.Component
import Verne.Types.Hashable

type Namespace = StrMap Component

lookupName :: String -> Namespace -> Maybe Component
lookupName = lookup

nameComponent :: String -> Component
nameComponent name =
  Component { id: hash ["Name Component", name]
            , name: name
            , signature: ["Namespace", "Component"]
            , exec: toForeign (resolveName name)
            , autocomplete: Just (toForeign nameCompletion)
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
            , autocomplete: Nothing
            }
