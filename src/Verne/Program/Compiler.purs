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
import Verne.Data.Part
import Verne.Data.Namespace
import Verne.Data.Type
import Verne.Program.Compiler.Coroutine
import Verne.Types.Hashable
import Verne.Types.Program
import Verne.Utils

import Debug.Trace

type Compile = CoT (Either Error Completion) Program

compile :: Syntax -> Program (Coroutine (Either Error Completion) Program Code)
compile = runCoT <<< go (TCon "IO ()")

-- String
go :: Type -> Syntax -> Compile Code
go t@(TCon "String") (Posi a b (Str str)) =
  pure $ Posc a b $ Obj $ valuePart t str

-- Overloaded String
go (TCon t1) syn@(Posi a b (Str str)) = do
  mobj <- lookupName t1
  let te = checkType (TCon t1) a b (TCon "String")
  func <- case mobj of
    Just obj@(Part {"type"=Type (TCon "String") (TCon t2)}) ->
      if t1 == t2 then pure obj else te
    _ -> te
  r <- go (TCon "String") syn
  pure $ Posc a b $ Obj $ curryPart construct r

-- Name lookup
go typ (Posi a b (Name name)) = do
  obj <- lookupName name
  let objType = case obj of Part {"type"=t} -> t
  checkType typ a b objType
  pure $ Posc a b (Obj obj)

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
  completeWith func args $ 
    checkType typ a b (typeFromArr (drop nArgs funcSig))
  pure $ Posc a b (Code (Posc a' b' (Obj func)) codeArgs)


checkType :: Type -> Int -> Int -> Type -> Compile Part
checkType t1 a b t2 =
  let msg = "Could not match expect type " ++ show t1 ++
            " with actual type " ++ show t2
  in when (t1 != t2) $ fail msg



lookupName :: String -> Compile Part
lookupName name = do
  name <- lift get <#> (\(Ps {globals}) -> lookup name globals)
  case name of
       Nothing -> fail (name ++ " is not defined")
       Just n -> return n



-- type Completion = {"object"::Part,pos::Pos}
-- -- | Provide a completion helper
-- completeWith :: Pos -> Part -> Compile Part
-- -- | If the component has a completion helper specified for this position
-- completeWith p c@(Part {autocomplete=Just _}) =
--   yield $ Right {pos:p,component:c}
-- -- | Check for a completion helper for the next position
-- completeWith _ c = pure unit
