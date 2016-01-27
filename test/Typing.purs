module Test.Typing where

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console

import Prelude

import Test.Unit (TIMER(), test, runTest)
import Test.Unit.Assert
import Test.Unit.Console (TESTOUTPUT())

import Language.Verne


foreign import registry :: TypeGraph

--main :: forall e. Eff ( testOutput :: TESTOUTPUT
--                      , avar :: AVAR
--                      , timer :: TIMER
--                      | e
--                      ) Unit


main =
  let result = parse "sayHi"
      lisp = case result of Success l -> l
      lispT = typeLisp registry "IO ()" lisp
  in print lispT
