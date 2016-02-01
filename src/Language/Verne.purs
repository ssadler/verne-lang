module Language.Verne
  ( isLeft
  ) where

import Data.Either hiding (isLeft)

isLeft :: forall a b. Either a b -> Boolean
isLeft = Data.Either.isLeft
