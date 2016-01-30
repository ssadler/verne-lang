module Language.Verne.TypeChecker
  ( typeLisp
  ) where

import Prelude

import Control.Monad.Reader

import Data.Array ((:), head, length, replicate, uncons, zipWith)
import Data.Array.Unsafe (tail)
import Data.Either
import Data.Generic
import Data.Maybe

import Language.Verne.Namespace
import Language.Verne.Types


foreign import toComponentOriginal :: forall a. a -> {}


-- todo: typed errors
typeLisp :: Namespace -> Type -> LISP Pos Atom -> LISP_T Atom
typeLisp ns typ' lisp = anno typ' lisp
  where
    anno :: Type -> LISP Pos Atom -> LISP_T Atom
    anno typ (LIST pos arr) = case uncons arr of
        Nothing -> LIST_T typ pos (Just "Empty expression not allowed") []
        Just { head: x, tail: xs } ->
            let atom = anno typ x
                typePadding = replicate (length xs) ""
            in  case atom of
                 ATOM_T _ _ _ (Left err) ->
                     LIST_T typ pos (Just err) (atom : zipWith anno typePadding xs)
                 ATOM_T _ _ _ (Right (Component com)) ->
                   if length com.signature -1 == length xs
                      then let sig = tail com.signature
                           in LIST_T typ pos Nothing (atom : zipWith anno sig xs)
                      else let err = Just (errArity com.name)
                               sig = tail (com.signature ++ typePadding)
                           in LIST_T typ pos err (atom : zipWith anno sig xs)

    anno typ (ATOM pos n@(Name name)) = ATOM_T typ pos n $
        case componentByName name ns  of
            Nothing -> Left ("Not defined: " ++ name)
            Just (Component component) ->
                case head component.signature of
                    Just t -> if t == typ then (Right $ Component component)
                                          else (errExpected typ t)
                    Nothing -> Left "Bad component signature"

    anno typ (ATOM pos s@(Str str)) = ATOM_T typ pos s $
        if typ == "String"
           then Right $ mkComponant "" ["String"] str
           else errExpected typ "String"

    errExpected typ t = Left $ "Couldn't match expected type " ++ typ ++ " with " ++ t
    errArity name = "Wrong number of arguments for " ++ name

    mkComponant name sig value = Component { name
                                           , signature: sig
                                           , original: toComponentOriginal value
                                           }
