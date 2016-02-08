module Language.Verne.TypeChecker
  ( LISP_T(..)
  , typeLisp
  ) where

import Prelude

import Data.Array ((:), head, length, replicate, uncons, zipWith)
import Data.Array.Unsafe (tail)
import Data.Either
import Data.Foreign
import Data.Maybe

import Language.Verne.Namespace
import Language.Verne.Types

-- | The typed lisp data structure


data LISP_T a = LIST_T { typ::Type
                       , pos::Pos
                       , merr::Maybe Error
                       , arr::Array (LISP_T a)
                       }
              | ATOM_T { typ::Type
                       , pos::Pos
                       , atom::a
                       , ecomp::Either Error Component
                       }

-- todo: typed errors
typeLisp :: Namespace -> Type -> AST -> LISP_T Atom
typeLisp ns typ' lisp = anno typ' lisp
  where
    anno :: Type -> AST -> LISP_T Atom
    anno typ (LIST pos arr) = case uncons arr of
        Nothing -> LIST_T {typ, pos, merr:(Just "Empty expression not allowed"), arr:[]}
        Just { head: x, tail: xs } ->
            let atom = anno typ x
                typePadding = replicate (length xs) ""
            in  case atom of
                 ATOM_T {ecomp=(Left err)} ->
                     LIST_T {typ, pos, merr:(Just err), arr:(atom : zipWith anno typePadding xs)}
                 ATOM_T {ecomp=(Right (Component com))} ->
                   if length com.signature -1 == length xs
                      then let sig = tail com.signature
                            in LIST_T {typ, pos, merr:Nothing, arr:(atom : zipWith anno sig xs)}
                      else let merr = Just (errArity com.name)
                               sig = tail (com.signature ++ typePadding)
                            in LIST_T {typ, pos, merr, arr:(atom : zipWith anno sig xs)}

    anno typ (ATOM pos atom@(Name name)) =
        let ecomp = case componentByName name ns of
                Nothing -> Left ("Not defined: " ++ name)
                Just (Component component) ->
                    case head component.signature of
                        Just t -> if t == typ then (Right $ Component component)
                                              else (errExpected typ t)
                        Nothing -> Left "Bad component signature"
        in ATOM_T {typ, pos, atom, ecomp}

    anno typ (ATOM pos atom@(Str str)) = 
        let ecomp = if typ == "String"
               then Right $ mkComponant "" ["String"] str
               else case strConstruct typ str of
                         Nothing -> errExpected typ "String"
                         Just comp -> Right comp
         in ATOM_T {typ, pos, atom, ecomp}

    errExpected typ t = Left $ "Couldn't match expected type " ++ typ ++ " with " ++ t
    errArity name = "Wrong number of arguments for " ++ name

    mkComponant name sig value = Component { name
                                           , signature: sig
                                           , autocomplete: Nothing
                                           , exec: toForeign (\_ -> value)
                                           }

    -- | For objects that can be overloaded by a string
    strConstruct typ str =
        componentByName typ ns >>=
            \(Component c) -> if c.signature == [typ, "String"]
                                  then Just (mkComponant "" [typ] str)
                                  else Nothing


instance eqLISP_T :: (Eq a) => Eq (LISP_T a) where
    eq (LIST_T {typ,pos,merr,arr}) (LIST_T {typ=typ2,pos=pos2,merr=merr2,arr=arr2}) =
        typ == typ2 && pos == pos2 && merr == merr2 && arr == arr2
    eq (ATOM_T {typ,pos,atom,ecomp}) (ATOM_T {typ=typ2,pos=pos2,atom=atom2,ecomp=ecomp2}) =
        typ == typ2 && pos == pos2 && atom == atom2 && ecomp == ecomp2
    eq (LIST_T _) (ATOM_T _) = false
    eq (ATOM_T _) (LIST_T _) = false

instance showLISP_T :: (Show a) => Show (LISP_T a) where
    show (LIST_T {typ,pos,merr,arr}) =
        "LIST_T {" <> typ <> "," <> show pos <> "," <> show merr <> "," <> show arr <> "}"
    show (ATOM_T {typ,pos,atom,ecomp}) =
        "ATOM_T {" <> typ <> "," <> show pos <> "," <> show atom <> "," <> show ecomp <> "}"
