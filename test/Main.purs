module Test.Main where

import Control.Monad.Aff.AVar (AVAR())

import Control.Monad.Eff (Eff())

import Data.Either

import Prelude

import Test.Unit (TIMER(), test, runTest)
import Test.Unit.Assert
import Test.Unit.Console (TESTOUTPUT())


import Language.Verne


main :: forall e. Eff ( testOutput :: TESTOUTPUT
                      , avar :: AVAR
                      , timer :: TIMER
                      | e
                      ) Unit
main = runTest do
  test "parser" do
    let expected = LIST (Pos 0 13) [ ATOM (Pos 0 1) (Name "a")
                                   , LIST (Pos 2 7)  [ ATOM (Pos 3   6) (Str  "b") ]
                                   , LIST (Pos 8 13) [ ATOM (Pos 9  10) (Name "c")
                                                     , ATOM (Pos 11 12) (Name "d")
                                                     ]
                                   ]
    equal (Right expected) (parse "a (\"b\") (c d)")
