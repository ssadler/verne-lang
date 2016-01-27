module Test.Main where

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())

import Prelude

import Test.Unit (TIMER(), test, runTest)
import Test.Unit.Assert
import Test.Unit.Console (TESTOUTPUT())

import Text.Parsing.StringParser hiding (Pos(..))

import Language.Verne


main :: forall e. Eff ( testOutput :: TESTOUTPUT
                      , avar :: AVAR
                      , timer :: TIMER
                      | e
                      ) Unit
main = runTest do
  test "basic success" do
    let expected = LIST (Pos 0 1) [ ATOM (Pos 0 1) (Name "a") ]
    equal (Success expected) (parse "a")
  test "complex success" do
    let expected = LIST (Pos 0 13) [ ATOM (Pos 0 1) (Name "a")
                                   , LIST (Pos 2 7)  [ ATOM (Pos 3   6) (Str  "b") ]
                                   , LIST (Pos 8 13) [ ATOM (Pos 9  10) (Name "c")
                                                     , ATOM (Pos 11 12) (Name "d")
                                                     ]
                                   ]
    equal (Success expected) (parse "a (\"b\") (c d)")
  test "user error" do
    equal (Failure 0 "Expected a lower case character but found ')'") (parse ")")
  test "just space" do
    equal (Partial (LIST (Pos 1 1) [ATOM (Pos 1 1) (Catch EndOfInput)])) (parse " ")
    
