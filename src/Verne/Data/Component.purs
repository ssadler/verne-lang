module Verne.Data.Component
  ( Component(..)
  , nullComponent
  , valueComponent
  ) where

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import Data.Maybe

import Verne.Types.Hashable
import Verne.Utils

import Prelude


newtype Component = Component
    { id :: String
    , name :: String
    , exec :: Foreign
    , signature :: Array String
    , autocomplete :: Maybe Foreign
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
            , signature: [typ]
            , exec: toForeign (\_ -> value)
            , autocomplete: Nothing
            }

nullComponent :: Component
nullComponent =
  Component { id: ""
            , name: "null"
            , signature: ["Null"]
            , exec: toForeign nullValue
            , autocomplete: Nothing
            }

