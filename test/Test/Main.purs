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
import Data.Tuple
import Partial.Unsafe (unsafeCrashWith)
import Verne.Data.Part
import Verne.Data.Program
import Verne.Data.Type
import Verne.Compiler
import Verne.Data.Code
import Verne.Parser
import Verne.Utils


foreign import dump :: forall a. a -> Eff (console :: CONSOLE) Unit


main :: Eff (console :: CONSOLE) Unit
main = do
  let out = runState testProg newProgramState
  dump out
  pure unit


testProg :: Program Code
testProg = do
  addPart nullPart
  addPart $ Part { id: "theAutocompleter"
                 , name: ""
                 , "type": TCon "Effect"
                 , exec: toForeign (\_ -> "")
                 , autocomplete: Just (toForeign (\_ -> ""))
                 , args: []
                 }
  code <- compile $ unsafeParse "null"
  pure code


unsafeParse :: String -> Syntax
unsafeParse str = case parse str of
                       Left (Tuple _ msg) -> unsafeCrashWith msg
                       Right syntax -> syntax


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
 
