module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert


import Data.Either
import Data.Foreign
import Verne.Data.Code
import Verne.Program
import Verne.Parser
import Verne.Utils
import Control.Monad.Except.Trans
import Control.Monad.State
import Control.Monad.Trans
foreign import titlePart :: Foreign
foreign import reloadPart :: Foreign


--main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = runTest do
  suite "test suite" do
    test "loll" do
      Assert.equal (Right 1) (evalState test1 newProgramState)
      let res = Right (Posi 0 infinity (Syntax (Posi 0 5 (Name "hello")) []))
      Assert.equal res (parse "hello")

test1 :: Program (Either String Int)
test1 = runExceptT $ do
  lift (addPart titlePart)
  pure 1
 
