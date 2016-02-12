module Verne.Types.Hashable
  ( Hash(..)
  , Hashable
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
