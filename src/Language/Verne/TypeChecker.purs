module Language.Verne.TypeChecker
  ( Component(..)
  , Error(..)
  , LISP_T(..)
  , Type(..)
  , TypeGraph(..)
  , typeLisp
  ) where

import Prelude

import Data.Array ((:), head, length, replicate, uncons, zipWith)
import Data.Array.Unsafe (tail)
import Data.Either
import Data.Generic
import Data.Maybe
import Data.StrMap

import Debug.Trace (spy)

import Language.Verne.Types

type Type = String

type Error = String

data LISP_T a = LIST_T Type Pos (Maybe Error) (Array (LISP_T a))
              | ATOM_T Type Pos a (Either Error Component)

derive instance genericLISP_T :: (Generic a) => Generic (LISP_T a)
instance eqLISP_T :: (Generic a, Eq a) => Eq (LISP_T a) where eq = gEq
instance showLISP_T :: (Generic a, Show a) => Show (LISP_T a) where show = gShow


data Component = Component { signature :: Array Type }

derive instance genericComponent :: Generic Component

type TypeGraph = { componentByName :: StrMap Component
                 , componentsByTypeHead :: StrMap (Array Component)
                 }

-- todo: typed errors
-- todo: component properties are extracted and verified at init time
typeLisp :: TypeGraph -> Type -> LISP Pos Atom -> LISP_T Atom
typeLisp graph typ lisp = anno typ lisp
  where
    anno :: Type -> LISP Pos Atom -> LISP_T Atom
    anno typ (LIST pos arr) =
        case uncons arr of
            Nothing -> LIST_T typ pos (Just "Empty expression not allowed") []
            Just { head: x, tail: xs } -> 
                let atom = anno typ x
                    typePadding = replicate (length xs) ""
                in  case atom of
                     -- cant check any of the list
                     ATOM_T _ _ _ (Left err) ->
                         LIST_T typ pos (Just err) (atom : zipWith anno typePadding xs)
                     -- LIST_T _ _ _ _          -> LIST_T typ pos (Just "oshit") arr
                     -- can check the list. If it's the wrong length, assign
                     -- an erorr to the list node. Use "()" as a substitute for
                     -- no type for now. 
                     ATOM_T _ _ _ (Right (Component com)) ->
                       if length com.signature -1 == length xs
                          then LIST_T typ pos Nothing (atom : zipWith anno com.signature xs)
                          else let err = Just (errArity "thingy")
                                   sig = tail (com.signature ++ typePadding)
                               in LIST_T typ pos err (atom : zipWith anno sig xs)

    anno typ (ATOM pos n@(Name name)) = ATOM_T typ pos n $
        case lookup name graph.componentByName of
            Nothing -> Left ("Not defined: " ++ name)
            Just (Component component) ->
                case head component.signature of
                    Just t -> if t == typ then (Right $ Component component)
                                          else (errExpected typ t)
                    Nothing -> Left ("Bad component signature")

    errExpected typ t = Left $ "Couldn't match expected type " ++ typ ++ " with " ++ t
    errArity name = "Wrong number of arguments for " ++ name



