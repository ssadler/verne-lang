module Language.Verne.Namespace where

import Prelude

import Data.Array.Unsafe (head)
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import qualified Data.Map as Map

import Language.Verne.Types


data Namespace = Namespace (Map.Map String Component) -- components by name
                           (Map.Map Type (Array Component)) -- components by type head


empty :: Namespace
empty = Namespace Map.empty Map.empty


componentByName :: String -> Namespace -> Maybe Component
componentByName ident (Namespace byName _) = Map.lookup ident byName


componentsByTypeHead :: Type -> Namespace -> Array Component
componentsByTypeHead typ (Namespace _ byTypeHead) =
    maybe [] id (Map.lookup typ byTypeHead)


addComponent :: Foreign -> Namespace -> Either String Namespace
addComponent f ns =
    case read f of
         Left err -> Left ("Error importing component: " ++ show err)
         Right component -> addComponent' component ns


addComponent' :: Component -> Namespace -> Either String Namespace
addComponent' comp@(Component c) (Namespace byName byTH) =
    if Map.member c.name byName
       then Left "Already registered"
       else let addition = Map.singleton (head c.signature) [comp]
                union = Map.unionWith (++) addition byTH
            in Right (Namespace (Map.insert c.name comp byName) union)

