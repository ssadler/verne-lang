module Verne.Data.Component
  ( Component(..)
  , nullComponent
  , valueComponent
  ) where

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import Data.Maybe

import Verne.Data.Type
import Verne.Types.Hashable
import Verne.Utils

import Prelude


newtype Component = Component
    { id :: String
    , name :: String
    , "type" :: Type
    , exec :: Foreign
    , autocomplete :: Maybe Foreign
    , args :: Array Component
    }

instance componentIsForeign :: IsForeign Component where
    read fo = Component <$> ({id:"", name:_, signature:_, exec:_, autocomplete:_}
                        <$> readProp "name" fo
                        <*> readProp "signature" fo
                        <*> readProp "exec" fo
                        <*> (runNullOrUndefined <$> readProp "autocomplete" fo)
                            )

instance showComponent :: Show Component where
    show (Component {id,name,signature,autocomplete,exec}) =
        let f = dump <$> autocomplete
            e = dump exec
         in "Component {" <> id <> ","
                          <> name <> ","
                          <> show signature <> ","
                          <> show f <> ","
                          <> dump exec <> "}"

instance eqComponent :: Eq Component where
    eq (Component c1) (Component c2) = c1.id == c2.id

valueComponent :: forall a. (Hashable a) => String -> a -> Component
valueComponent typ value =
  Component { id: hash value
            , name: dump value
            , "type": TCon typ
            , exec: toForeign (\_ -> value)
            , autocomplete: Nothing
            , args: []
            }

nullComponent :: Component
nullComponent =
  Component { id: ""
            , name: "null"
            , "type": TCon "Null"
            , exec: toForeign nullValue
            , autocomplete: Nothing
            , args: []
            }

curry :: Component -> Component -> Component
curry (Component c1@{"type":Type _ t) arg =
  Component { id: hash [c1,c2]
            , name: ""
            , "type": t
            , exec: c1.exec
            , autocomplete: c1.autocomplete
            , args: snoc c1.args arg
            }
