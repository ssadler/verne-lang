module Verne.Data.Part
  ( Part(..)
  , unsafeCurryPart
  , nullPart
  , valuePart
  ) where

import Data.Array (snoc)
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
import Data.Maybe

import Verne.Data.Type
import Verne.Data.Hashable
import Verne.Utils

import Prelude


newtype Part = Part
    { id :: String
    , name :: String
    -- TODO: , "module" :: String
    , "type" :: Type
    , exec :: Foreign
    , autocomplete :: Maybe Foreign
    , args :: Array Part
    }

-- | How to execute
--
-- In order that part execution is easy and unambiguous,
-- exec functions should be wrapped in closures that expect
-- the part as the `this` variable.
-- They are then able to recursively execute themselves.
-- The `this` variable refers to the execution API,
-- which itself is typed.
instance partIsForeign :: IsForeign Part where
    read fo = do
      n <- readProp "name" fo
      t <- readProp "type" fo
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

instance hashPart :: Hashable Part where
  hash (Part {id}) = id


valuePart :: forall a. (Hashable a) => Type -> a -> Part
valuePart typ value =
  Part { id: hash value
       , name: dump value
       , "type": typ
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

unsafeCurryPart :: Part -> Part -> Part
unsafeCurryPart (Part c1@{"type":Type _ t}) arg =
  Part { id: hash [Part c1,arg]
       , name: ""
       , "type": t
       , exec: c1.exec
       , autocomplete: c1.autocomplete
       , args: snoc c1.args arg
       }
