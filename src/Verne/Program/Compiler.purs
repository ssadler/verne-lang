module Verne.Program.Compiler
  ( compile
  ) where

import Control.Monad (when)
import Control.Monad.Except.Trans

import Data.Alt
import Data.Array (foldM, drop, length, last, take, uncons)
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

import Debug.Trace

type Completion = {component::Component,pos::Pos}
type Compile = CoT (Either Error Completion) Program

compile :: Int -> Code -> Program (Coroutine (Either Error Completion) Program Component)
compile caret = runCoT <<< go caret ["IO ()"]

go :: Int -> Array Type -> Code -> Compile Component
go caret typ (Atom {pos,component}) = do
  c <- lift $ resolve typ component
  when (caret >= pos.a && caret <= pos.b)
       (completeWith pos c)
  pure c
go caret typ (List {pos={b},head,args}) = do
  ch <- go caret typ head
  com <- curry caret typ ch (uncons args)
  case getPos <$> last args of
    Just {b} | caret > b + 1 -> completeWith {a:caret,b:caret} com
    _ -> pure unit
  pure com

curry :: Int -> Array Type -> Component
      -> Maybe {head::Code,tail::Array Code}
      -> Compile Component
curry _ _ com Nothing = pure com
curry caret typ (Component c1) (Just {head,tail}) = do
  arg <- go caret typ head
  c2 <- construct (take 1 c1.signature) signature
  let c2 = case arg of Component c -> c

  matchType (take 1 c1.signature) c2.signature
  let c3 = { id: hashMany [c1.id, c2.id]
           , name: ""
           , signature: drop 1 c1.signature
           , exec: curryForeign c1.exec c2.exec
           , autocomplete: autoCurry c2.exec <$> c1.autocomplete 
           }
  curry caret typ (Component c3) $ uncons tail

-- | Match types. Supports string overloading.
matchType :: Array Type -> Array Type -> Compile Unit
matchType exptected actual = do
  globals <- gets (\(Ps {globals}) -> globals)
  targetType = U.last expected
  let matched = expected == actual
             || lookupName targetType globals 
  when (not matched) do
     yield (Left "Type error, expected: " ++ show expected ++
                 " But got: " ++ show actual)

-- | Provide a completion helper
completeWith :: Pos -> Component -> Compile Unit
-- | If the component has a completion helper specified for this position
completeWith p c@(Component {autocomplete=Just _}) =
  yield $ Right {pos:p,component:c}
-- | Check for a completion helper for the next position
completeWith _ c = pure unit

-- | Related to component resolution
--
resolve :: Array Type -> Component -> Program Component
resolve typ (Component {name,signature=["_lookup"]}) = do
  globals <- gets (\(Ps {globals}) -> globals)
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


orM :: forall m. -> m Boolean -> m Boolean -> m Boolean
orM a b = do
  a' <- a
  if a' then pure true else b
