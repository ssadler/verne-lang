module Verne.Compiler
  ( compile
  ) where

import Control.Monad.State.Class

import Data.Array ( foldM, drop, head, length, last
                  , reverse, snoc, take, uncons, zipWith)
import Data.Maybe
import Data.Traversable

import Partial.Unsafe (unsafeCrashWith)
import Prelude

import Verne.Data.Code
import Verne.Data.Part
import Verne.Data.Program
import Verne.Data.Type
import Verne.Utils

-- | Compile goes from ParsedCode to WiredCode

type Compile = Program

compile :: Syntax -> Program Code
compile = go (TCon "Effect")

-- String
go :: Type -> Syntax -> Compile Code
go typ@(TCon "String") (Str str) =
  pure $ Atom $ valuePart typ str
go _ _ = unsafeCrashWith "go: unhandled"

-- Overloaded String
go typ@(TCon t1) (Str str) =
  deref typ t1 $ \part ->
    case part of
      Part {"type":Type (TCon "String") (TCon t2)} ->
        -- TODO: Will produce confusing error if type fails
        checkType typ (TCon t2) $
          let strPart = valuePart (TCon "String") str
          in pure $ Atom $ unsafeCurryPart part strPart
      _ -> pure $ TypeError typ (TCon "String")

-- Name lookup
go typ (Name name) =
  deref typ name $ \part ->
    let actual = case part of Part {"type":t} -> t
    in checkType typ actual $ pure $ Atom part

-- Function call
go typ (Syntax (Posi a' b' (Name name)) synArgs) = do
  -- In order to support Syntax as function we'll probably need to implement
  -- HM or at least have a routine to infer the type of the expression.
  -- But thats a luxury problem for now.
  -- We can also implement our own shitty type system by having type vars
  -- and unification functions. That way, we can propagate upwards and
  -- downwards. Which is probably most of what HM does.
  deref typ name $ \func -> do
    let funcSig = case func of Part {"type":t} -> typeToArr t
        nDiff = (length synArgs + typeLength typ) - length funcSig
    checkTooManyArgs nDiff synArgs $ \synArgs -> do
      codeArgs <- sequence $ zipWith go funcSig synArgs
      let nargs = length synArgs
          actualType = typeFromArr (drop nargs funcSig)
          code = Code (Posc a' b' (Atom func)) codeArgs
      case getNextArgument typ actualType of
        Just t -> pure (addNeedsArgument t code)
        Nothing -> checkType typ actualType (pure code)

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

addNeedsArgument :: Type -> Code -> Code
addNeedsArgument typ (Code func args) =
  let na b = Posc (b+1) infinity (NeedsArgument typ)
   in case maybe func id (last args) of
           Posc _ b _ -> Code func (snoc args (na b))
           _ -> unsafeCrashWith "addNeedsArgument.inner: unhandled"
addNeedsArgument _ _ = unsafeCrashWith "addNeedsArgument: unhandled"
