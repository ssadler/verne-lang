module Test.Typing where

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console

import Data.Array (foldM)
import Data.Either.Unsafe
import Data.Foreign

import Prelude

import Test.Unit (TIMER(), test, runTest)
import Test.Unit.Assert
import Test.Unit.Console (TESTOUTPUT())

import qualified Language.Verne.Namespace as NS
import Language.Verne.Parser
import Language.Verne.Types
import Language.Verne.TypeChecker


--main :: forall e. Eff ( testOutput :: TESTOUTPUT
--                      , avar :: AVAR
--                      , timer :: TIMER
--                      | e
--                      ) Unit

foreignComponents :: Array Foreign
foreignComponents = toForeign <$>
    [ {name:"sayHi",signature:["IO ()"]}
    , {name:"setBackgroundColor",signature:["IO ()","String"]}]

ns :: NS.Namespace
ns = fromRight $ foldM (flip NS.addComponent) NS.empty foreignComponents

main = do
  let result = parse "sayHi"
      lisp = case result of Success l -> l
      lispT = typeLisp ns "IO ()" lisp
  print lispT

  let result = parse "setBackgroundColor \"#FFF\""
      lisp = case result of Success l -> l
      lispT = typeLisp ns "IO ()" lisp
  print lispT
