module Verne.Program.Compiler
  ( compile
  ) where

import Control.Monad (when)
import Control.Monad.Except.Trans

import Data.Array (foldM, drop, last, take, uncons)
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
compile caret = runCoT <<< go caret ["IO ()"]

go :: Int -> Array Type -> Code -> Compile Component
go caret typ (Atom {pos,component}) = do
  c <- lift $ resolve typ component
  when (caret >= pos.a && caret <= pos.b)
       (provideCompletion c)
  pure c
go caret typ (List {head,args}) = do
  ch <- go caret typ head
  com <- curry caret typ ch (uncons args)
  case getPos <$> last args of
    Just {b} | caret > b + 1 -> provideCompletion com
  pure com


curry :: Int -> Array Type -> Component
      -> Maybe {head::Code,tail::Array Code}
      -> Compile Component
curry _ _ com Nothing = pure $ com
curry caret typ (Component c1) (Just {head,tail}) = do
  arg <- go caret typ head
  let c2 = case arg of Component c -> c
  when (take 1 c1.signature /= c2.signature)
       (yield (Left "curry mismatch"))
  let c3 = { id: hashMany [c1.id, c2.id]
           , name: ""
           , signature: drop 1 c1.signature
           , exec: curryForeign c1.exec c2.exec
           , autocomplete: autoCurry c2.exec <$> c1.autocomplete 
           }
  curry caret typ (Component c3) $ uncons tail


provideCompletion :: Component -> Compile Unit
provideCompletion c@(Component {autocomplete=Just _}) = yield $ Right c
provideCompletion c = pure unit

-- | Related to component resolution
--
resolve :: Array Type -> Component -> Program Component
resolve typ (Component {name,signature=["_lookup"]}) = do
  globals <- get <#> (\(PS {globals}) -> globals)
  let getOptions = toForeign (\_ -> getNameCompletions typ name globals)
  pure $ maybe (cantResolve name getOptions) id $ lookup name globals
resolve _ c = pure c
 
cantResolve :: String -> Foreign -> Component
cantResolve name exec =
  Component { id : hash ["nameLookup", name]
            , name: ""
            , signature: [""]
            , exec: exec
            , autocomplete: Just (toForeign "names")
            }

