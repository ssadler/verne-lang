module Verne.Program.Compiler.Coroutine where

import Prelude

import Control.Monad.Trans (MonadTrans, lift)

import Data.Maybe

import Debug.Trace

newtype CoT r m a = CoT (m (Coroutine r m a))

data Coroutine r m a = Yield (CoT r m a) r | Run a

runCoT :: forall r m a. CoT r m a -> m (Coroutine r m a)
runCoT (CoT a) = a

yield :: forall r m. (Monad m) => r -> CoT r m Part
yield = CoT <<< pure <<< Yield (pure unit)

instance fA :: (Functor m) => Functor (Coroutine r m) where
  map f (Run a) = Run (f a)
  map f (Yield cont b) = Yield (map f cont) b


instance fAT :: (Functor m) => Functor (CoT r m) where
  map f (CoT a) = CoT $ ((<$>) ((<$>) f)) a

instance fMT :: MonadTrans (CoT r) where
  lift m = CoT $ Run <$> m

instance aA :: (Monad m) => Apply (CoT r m) where
  apply = ap

instance cA :: (Monad m) => Applicative (CoT r m) where
  pure = CoT <<< pure <<< Run

instance bA :: (Monad m) => Bind (CoT r m) where
  bind (CoT ma) f = CoT $ do
    co <- ma
    case co of
      Run a -> runCoT (f a)
      Yield cont r -> pure (Yield (bind cont f) r)

instance mA :: (Monad m) => Monad (CoT r m)

