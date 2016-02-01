module Language.Verne.Utils where

foreign import compactShow :: String -> String

foreign import isSame :: forall a. a -> a -> Boolean
