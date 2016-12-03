module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Control.Monad.Except.Trans
import Control.Monad.State
import Control.Monad.Trans
import Data.Either
import Data.Foreign
import Data.Maybe
import Verne.Program
import Verne.Data.Part
import Verne.Data.Type
import Verne.Compiler
import Verne.Data.Code
import Verne.Parser
import Verne.Utils



main :: Eff (console :: CONSOLE) Unit
main = do
  let out = runState $ runExceptT testProg
  pure unit


testProg :: ExceptT String Program Unit
testProg = do
  lift $ addPart nullPart
  lift $ addPart $ Part { id: "theAutocompleter"
                        , name: ""
                        , "type": TCon "Effect"
                        , exec: toForeign (\_ -> "")
                        , autocomplete: Just (toForeign (\_ -> ""))
                        , args: []
                        }
  pure unit


--main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
--main = runTest do
--  suite "lol" do
--    test "test parse" do
--      Assert.equal (Right syntax) (parse "hello")
--    test "test full" do
--
--
--            runExceptT $ do
--              lift (addPart titlePart)
--            compile syntax
--            Assert.equal (Atom nullPart) code
--      Assert.equal 1 1
--  where
--    syntax = Posi 0 infinity (Syntax (Posi 0 5 (Name "hello")) [])

--test1 :: Program (Either String Int)
--test1 = runExceptT $ do
--  pure 1
 
