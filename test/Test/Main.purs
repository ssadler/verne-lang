module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT, assert)


main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  assert $ 1 == 1
