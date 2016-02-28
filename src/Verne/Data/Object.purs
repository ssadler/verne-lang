module Verne.Data.Object
  ( Object(..)
  , nullObject
  , valueObject
  ) where

import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import Data.Maybe

import Verne.Data.Type
import Verne.Types.Hashable
import Verne.Utils

import Prelude


newtype Object = Object
    { id :: String
    , name :: String
    , "type" :: Type
    , exec :: Foreign
    , autocomplete :: Maybe Foreign
    , args :: Array Object
    }

instance objectIsForeign :: IsForeign Object where
    read fo = Object <$> ({id:"", name:_, "type":_, exec:_, autocomplete:_}
                        <$> readProp "name" fo
                        <*> readProp "type" fo
                        <*> readProp "exec" fo
                        <*> (runNullOrUndefined <$> readProp "autocomplete" fo)
                            )

instance showObject :: Show Object where
    show (Object {id,name,"type"=t,autocomplete,exec}) =
        let f = dump <$> autocomplete
            e = dump exec
         in "Object {" <> id <> ","
                          <> name <> ","
                          <> show t <> ","
                          <> show f <> ","
                          <> dump exec <> "}"

instance eqObject :: Eq Object where
    eq (Object c1) (Object c2) = c1.id == c2.id

valueObject :: forall a. (Hashable a) => String -> a -> Object
valueObject typ value =
  Object { id: hash value
            , name: dump value
            , "type": TCon typ
            , exec: toForeign (\_ -> value)
            , autocomplete: Nothing
            , args: []
            }

nullObject :: Object
nullObject =
  Object { id: ""
            , name: "null"
            , "type": TCon "Null"
            , exec: toForeign nullValue
            , autocomplete: Nothing
            , args: []
            }

curry :: Object -> Object -> Object
curry (Object c1@{"type":Type _ t) arg =
  Object { id: hash [c1,c2]
            , name: ""
            , "type": t
            , exec: c1.exec
            , autocomplete: c1.autocomplete
            , args: snoc c1.args arg
            }
