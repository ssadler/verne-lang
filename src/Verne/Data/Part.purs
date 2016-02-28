module Verne.Data.Part
  ( Part(..)
  , curryPart
  , nullPart
  , valuePart
  ) where

import Data.Array (snoc)
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import Data.Maybe

import Verne.Data.Type
import Verne.Types.Hashable
import Verne.Utils

import Prelude


newtype Part = Part
    { id :: String
    , name :: String
    , "type" :: Type
    , exec :: Foreign
    , autocomplete :: Maybe Foreign
    , args :: Array Part
    }

instance objectIsForeign :: IsForeign Part where
    read fo = do
      n <- readProp "name" fo
      t <- parseType <$> readProp "type" fo
      e <- readProp "exec" fo
      auto <- runNullOrUndefined <$> readProp "autocomplete" fo
      pure $ Part {id:"", name:n, "type":t, exec:e, autocomplete:auto, args:[]}

-- instance showPart :: Show Part where
--     show (Part {id,name,"type"=t,autocomplete,exec}) =
--         let f = dump <$> autocomplete
--             e = dump exec
--          in "Part {" <> id <> ","
--                           <> name <> ","
--                           <> show t <> ","
--                           <> show f <> ","
--                           <> dump exec <> "}"

instance eqPart :: Eq Part where
    eq (Part c1) (Part c2) = c1.id == c2.id

valuePart :: forall a. (Hashable a) => String -> a -> Part
valuePart typ value =
  Part { id: hash value
       , name: dump value
       , "type": TCon typ
       , exec: toForeign (\_ -> value)
       , autocomplete: Nothing
       , args: []
       }

nullPart :: Part
nullPart =
  Part { id: ""
       , name: "null"
       , "type": TCon "Null"
       , exec: toForeign nullValue
       , autocomplete: Nothing
       , args: []
       }

curryPart :: Part -> Part -> Part
curryPart (Part c1@{"type":Type _ t}) arg =
  Part { id: hash [c1,arg]
       , name: ""
       , "type": t
       , exec: c1.exec
       , autocomplete: c1.autocomplete
       , args: snoc c1.args arg
       }
