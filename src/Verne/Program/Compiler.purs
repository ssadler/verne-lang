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
import Verne.Data.Type
import Verne.Program.Compiler.Coroutine
import Verne.Types.Hashable
import Verne.Types.Program
import Verne.Utils

import Debug.Trace

type Completion = {component::Component,pos::Pos}
type Compile = CoT (Either Error Completion) Program

compile :: Syntax -> Program (Coroutine (Either Error Completion) Program Code)
compile = runCoT <<< go (TCon "IO ()")

-- String
go :: Type -> Syntax -> Compile Code
go t@(TCon "String") (Posi a b (Str str)) =
  pure $ Posc a b $ Atom $ valueComponent t str

-- Overloaded String
go (TCon t1) syn@(Posi a b (Str str)) = do
  mconstruct <- lookupName t1
  let te = typeError t1 a b (TCon "String")
  construct <- case mconstruct of
    Just c@(Component {"type"=Type (TCon "String") (TCon t2)}) ->
      if t1 != t2 then te else c
    _ -> te
  let r = go (TCon "String") syn
  pure $ Posc a b $ Atom $ curry construct r

-- Name lookup
go typ (Posi a b (Name name)) = do
  comp <- lookupName name
  let compSig = case comp of Component {signature} -> signature
  checkType typ a b compSig
  pure $ Posc a b (Atom comp)

-- Function call
go typ (Posi a b (Syntax (Posi a' b' (Name name)) args)) = do
  -- In order to support Syntax as function we'll probably need to implement
  -- HM or at least have a routine to infer the type of the expression.
  -- But thats a luxury problem for now.
  func <- lookupName name
  let funcSig = case func of Component {signature} -> typeToArr signature
      nArgs = length args
  when (nArgs > length funcSig - 1) do
     fail ("Too many arguments for function " ++ name)
  checkType typ a b (typeFromArr (drop nArgs funcSig))
  codeArgs <- sequence $ zipWith go funcSig args
  pure $ Posc a b (Code (Posc a' b' (Atom func)) codeArgs)





lookupName :: String -> Compile Component
lookupName name = do
  name <- lift get <#> (\(Ps {globals}) -> lookup name globals)
  case name of
       Nothing -> fail (name ++ " is not defined")
       Just n -> return n



-- -- | Provide a completion helper
-- completeWith :: Pos -> Component -> Compile Unit
-- -- | If the component has a completion helper specified for this position
-- completeWith p c@(Component {autocomplete=Just _}) =
--   yield $ Right {pos:p,component:c}
-- -- | Check for a completion helper for the next position
-- completeWith _ c = pure unit
