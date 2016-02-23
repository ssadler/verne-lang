module Verne.Program.Compiler
  ( compile
  ) where

import Control.Monad (when)
import Control.Monad.Except.Trans

import Data.Array (foldM, drop, last, take)
import Data.Either
import Data.Foreign
import Data.Maybe
import Data.StrMap (lookup)
import Data.Traversable

import Prelude

import Verne.Data.Code
import Verne.Data.Component
import Verne.Data.Namespace
import Verne.Program.Compiler.Coroutine
import Verne.Types.Hashable
import Verne.Types.Program
import Verne.Utils


type Compile = CoT (Either Error Component) Program

compile :: Int -> Code -> Program (Coroutine (Either Error Component) Program Component)
compile caret = runCoT <<< go caret

go :: Int -> Type -> Code -> Compile Component
go caret typ (Atom {pos,component}) = do
  c <- lift $ resolve component
  when (caret >= pos.a && caret <= pos.b)
       (provideCompletion typ c)
  pure c
go caret typ (List {head,args}) = do
  ch <- go caret typ head
  let argtypes = case ch of Component {signature} -> signature
  ct <- traverse (go caret typ) args
  com <- foldM curry ch ct
  case getPos <$> last args of
    Just {b} | caret > b + 1 -> provideCompletion com
  pure com

curry :: Component -> Component -> Compile Component
curry (Component c1) (Component c2) = Component <$> do
  when (take 1 c1.signature /= c2.signature)
       (yield (Left "curry mismatch"))
  pure { id: hashMany [c1.id, c2.id]
       , name: ""
       , signature: drop 1 c1.signature
       , exec: curryForeign c1.exec c2.exec
       , autocomplete: autoCurry c2.exec <$> c1.autocomplete 
       }

provideCompletion :: Component -> Compile Unit
provideCompletion c@(Component {autocomplete=Just _}) = yield $ Right c
provideCompletion c = pure unit

-- | Related to component resolution
--
resolve :: Component -> Program Component
resolve (Component {name,signature=["_lookup"]}) = do
  globals <- get <#> (\(PS {globals}) -> globals)
  pure $ maybe (cantResolve name) id $ lookup name globals
resolve c = pure c
 
cantResolve :: String -> Component
cantResolve name =
  Component { id : hash ["Can't resolve", name]
            , name: ""
            , signature: [""]
            , exec: toForeign (\a -> a)
            , autocomplete: Just (toForeign nameCompletion)
            }

