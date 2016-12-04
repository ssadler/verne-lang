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
import Verne.Compiler
import Verne.Data.Part
import Verne.Data.Program
import Verne.Data.Type
import Verne.Data.Code
import Verne.Exec
import Verne.Parser


foreign import dump :: forall a. a -> Unit


main :: Eff (console :: CONSOLE) Unit
main = do
  pure $ runState testProg newProgramState
  pure unit



testProg :: Program Unit
testProg = do
  addPart nullPart
  addPart $ Part { id: ""
                 , name: "thingToEffect"
                 , "type": unsafeParseType "Thing -> Effect"
                 , exec: toForeign (\_ -> "")
                 , autocomplete: Nothing
                 , args: []
                 }
  addPart $ Part { id: ""
                 , name: "stringToThing"
                 , "type": unsafeParseType "String -> Thing"
                 , exec: toForeign (\_ -> "")
                 , autocomplete: Just (toForeign (\_ -> ""))
                 , args: []
                 }
  addPart $ Part { id: ""
                 , name: "thing"
                 , "type": unsafeParseType "Thing"
                 , exec: toForeign (\_ -> "")
                 , autocomplete: Just (toForeign (\_ -> ""))
                 , args: []
                 }
  code <- compile $ unsafeParse "thingToEffect "
  --pure $ dump code
  completion <- getCompletions 14 code
  pure $ dump completion


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
 
