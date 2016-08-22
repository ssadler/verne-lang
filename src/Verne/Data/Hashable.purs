module Verne.Data.Hashable
  ( Hash(..)
  , class Hashable
  , hash
  ) where

import Verne.Utils

import Prelude

type Hash = String

class Hashable a where
  hash :: a -> Hash

instance hashString :: Hashable String where
  hash = hashOne

instance hashInt :: Hashable Int where
  hash = hashOne <<< show

instance hashArray :: (Hashable a) => Hashable (Array a) where
  hash = hashMany <<< map hash
