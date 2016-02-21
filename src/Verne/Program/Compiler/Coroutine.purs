module Verne.Program.Compiler.Coroutine where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Maybe

import Debug.Trace

newtype CoT r m a = CoT (m (Coroutine r m a))

data Coroutine r m a = Yield (CoT r m a) r | Run a

mapCoT f (CoT a) = CoT (f a)
runCoT (CoT a) = a

instance fA :: (Functor m) => Functor (Coroutine r m) where
  map f (Run a) = Run (f a)
  map f (Yield cont b) = Yield (map f cont) b

instance fAT :: (Functor m) => Functor (CoT r m) where
  map f = mapCoT ((<$>) ((<$>) f))

instance aA :: (Monad m) => Apply (CoT r m) where
    apply = ap
--  apply (Run f) b = apply f b
--  apply (Yield cont b) a = Yield (apply cont a) b

instance cA :: (Monad m) => Applicative (CoT r m) where
  pure = CoT <<< pure <<< Run

instance bA :: (Monad m) => Bind (CoT r m) where
  bind (CoT ma) f = CoT $ do
    co <- ma
    case co of
      Run a -> runCoT (f a)
      Yield cont r -> pure (Yield (bind cont f) r)


instance mA :: (Monad m) => Monad (CoT r m)

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = case test of
--             Run b -> log b
--             Yield cont a -> case cont of
--                                  Run b -> log b

