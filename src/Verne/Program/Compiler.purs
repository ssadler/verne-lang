module Verne.Program.Compiler
  ( CompileError(..)
  , compile
  ) where

import Control.Alt
import Control.Apply
import Control.Monad (when)
import Control.Monad.Except.Trans

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

type Compile = ExceptT CompileError Program

data CompileError = CompileError String
                  | NeedArg Code (Array Code)

compile :: Syntax -> Program (Either CompileError Code)
compile = runExceptT <<< go (TCon "IO ()")

-- String
go :: Type -> Syntax -> Compile Code
go t@(TCon "String") (Posi a b (Str str)) =
  pure $ Posc a b $ Atom $ valuePart t str

-- Overloaded String
go (TCon t1) syn@(Posi a b (Str str)) = do
  let te = typeError (TCon t1) a b (TCon "String")
  part <- lookupName t1
  case part of
    Part {"type"=Type (TCon "String") (TCon t2)} ->
      if t1 /= t2 then te else do
        let strPart = valuePart (TCon "String") str
        pure $ Posc a b $ Atom $ unsafeCurryPart part strPart
    _ -> te

-- Name lookup
go typ (Posi a b (Name name)) = do
  obj <- lookupName name
  let objType = case obj of Part {"type"=t} -> t
  when (typ /= objType) (typeError typ a b objType)
  pure $ Posc a b (Atom obj)

-- Function call
go typ (Posi a b (Syntax (Posi a' b' (Name name)) args)) = do
  -- In order to support Syntax as function we'll probably need to implement
  -- HM or at least have a routine to infer the type of the expression.
  -- But thats a luxury problem for now.
  func <- lookupName name
  let funcSig = case func of Part {"type"=t} -> typeToArr t
      nArgs = length args
  when (nArgs > length funcSig - 1) do
     fail ("Too many arguments for function " ++ name)
  codeArgs <- sequence $ zipWith go funcSig args
  --completeWith (Posc a b func) args $ 
  --  checkType typ a b (typeFromArr (drop nArgs funcSig))
  let actualType = typeFromArr (drop nArgs funcSig)
  when (typ /= actualType) (typeError typ a b actualType)
  pure $ Posc a b (Code (Posc a' b' (Atom func)) codeArgs)


typeError :: forall a. Type -> Int -> Int -> Type -> Compile a
typeError t1 a b t2 =
  let msg = "Could not match expect type " ++ show t1 ++
            " with actual type " ++ show t2
  in fail msg

completeWith :: Code -> Array Code -> Compile Unit
completeWith a b = pure unit

lookupName :: String -> Compile Part
lookupName name = do
  mpart <- lift get <#> (\(Ps {globals}) -> lookup name globals)
  case mpart of
       Nothing -> fail (name ++ " is not defined")
       Just part -> pure part

fail :: forall a. Error -> Compile a
fail = ExceptT <<< return <<< Left <<< CompileError
