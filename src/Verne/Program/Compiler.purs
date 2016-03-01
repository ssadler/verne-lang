module Verne.Program.Compiler
  ( compile
  ) where

import Control.Alt
import Control.Apply
import Control.Monad (when)
import Control.Monad.Except.Trans
import Control.Monad.State.Class

import Data.Array (foldM, drop, length, last, take, uncons, zipWith)
import Data.Either
import Data.Foreign
import Data.Maybe
import Data.StrMap (lookup)
import Data.Traversable

import Prelude

import Verne.Data.Code
import Verne.Data.Part
import Verne.Data.Namespace
import Verne.Data.Type
import Verne.Types.Hashable
import Verne.Types.Program
import Verne.Utils

import Debug.Trace

-- | Compile goes from ParsedCode to WiredCode

type Compile = Program

compile :: Syntax -> Program Code
compile = go (TCon "Effect")

-- String
go :: Type -> Syntax -> Compile Code
go typ@(TCon "String") (Str str) =
  pure $ Atom $ valuePart typ str

-- Overloaded String
go typ@(TCon t1) (Str str) =
  deref typ t1 $ \part ->
    case part of
      Part {"type"=Type (TCon "String") (TCon t2)} ->
        -- TODO: Will produce confusing error if type fails
        checkType typ (TCon t2) $
          let strPart = valuePart (TCon "String") str
          in pure $ Atom $ unsafeCurryPart part strPart
      _ -> pure $ TypeError typ (TCon "String")

-- Name lookup
go typ (Name name) =
  deref typ name $ \part ->
    let actual = case part of Part {"type"=t} -> t
    in checkType typ actual $ pure $ Atom part

-- Function call
go typ (Syntax (Posi a' b' (Name name)) args) = do
  -- In order to support Syntax as function we'll probably need to implement
  -- HM or at least have a routine to infer the type of the expression.
  -- But thats a luxury problem for now.
  -- We can also implement our own shitty type system by having type vars
  -- and unification functions. That way, we can propagate upwards and
  -- downwards. Which is probably most of what HM does.
  deref typ name $ \func -> do
    let funcSig = case func of Part {"type"=t} -> typeToArr t
        nDiff = (length args + typeLength typ) - length funcSig

    checkTooManyArgs nDiff args $ \args -> do
      codeArgs <- sequence $ zipWith go funcSig args
      let nargs = length args
          actualType = typeFromArr (drop nargs funcSig)
      checkType typ actualType $
        pure $ Code (Posc a' b' (Atom func)) codeArgs

-- Position wrapper
go typ (Posi a b code) = Posc a b <$> go typ code

-- | Dereference a name then continue
deref :: Type -> String -> (Part -> Compile Code) -> Compile Code
deref typ name act = do
  mpart <- get <#> (\(Ps {globals}) -> lookup name globals)
  case mpart of 
       Just part -> act part
       Nothing -> pure (Undefined name typ)

-- | Check type then continue
checkType :: Type -> Type -> Compile Code -> Compile Code
checkType t1 t2 act = if t1 /= t2 then pure (TypeError t1 t2) else act

-- | Truncate argument list then continue
checkTooManyArgs :: Int -> Array Syntax -> (Array Syntax -> Compile Code) -> Compile Code
checkTooManyArgs diff args act = do
  if diff < 1
     then act args
     else
       let keep = length args - diff
        in TooManyArguments (drop keep args) <$> act (take keep args)
