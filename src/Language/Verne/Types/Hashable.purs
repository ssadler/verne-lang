module Language.Verne.Types.Hashable
  ( Hash(..)
  , Hashable
  , hash
  ) where

import Language.Verne.Utils

import Control.Alt
import Control.Apply
import Prelude

type Hash = String

class Hashable a where
  hash :: a -> Hash

instance hashString :: Hashable String where
  hash = hashOne

instance hashInt :: Hashable Int where
  hash = hashOne <<< show
