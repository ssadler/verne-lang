module Verne.Program.Compiler
  ( compile
  ) where

import Prelude

import Data.Array ((:), head, length, replicate, uncons, zipWith)
import Data.Array.Unsafe (tail)
import Data.Either
import Data.Foreign
import Data.Maybe
import Data.StrMap (lookup)

import Verne.Types


-- todo: typed errors
compile :: Namespace -> Type -> AST -> Code
compile ns typ' ast = anno typ' ast
  where
  anno :: Type -> AST -> Code
  anno typ (SList {pos,arr}) = case uncons arr of
    Just { head: x, tail: xs } ->
      let code = anno typ x
       in case code of
         Atom {component=Right (Component com)} -> 
           let typePadding = replicate (length xs) ""
               sig = tail (com.signature ++ typePadding)
               error = if length com.signature -1 == length xs
                         then Nothing
                         else Just (errArity com.name)
            in List {typ,pos,error,arr:(code : zipWith anno sig xs)}
         List _ -> List {typ,pos,error:Just "List as function not yet supported",arr:[]}
    Nothing -> List {typ,pos,error:Just "Empty expression not allowed",arr:[]}

  anno typ (SAtom {pos,atom=atom@(Name name)}) =
    let component = case componentByName name ns of
                      Nothing -> Left ("Not defined: " ++ name)
                      Just (Component compo) ->
                        case head compo.signature of
                          Just t -> if t == typ then (Right $ Component compo)
                                                else (errExpected typ t)
                          Nothing -> Left "Bad component signature"
    in Atom {typ,pos,atom,component}

  anno typ (SAtom {pos,atom=atom@(Str str)}) =
    let component = if typ == "String"
         then Right $ valueComponent ["String"] str
         else case strConstruct typ str of
             Nothing -> errExpected typ "String"
             Just comp -> Right comp
     in Atom {typ, pos, atom, component}

  errExpected typ t = Left $ "Couldn't match expected type " ++ typ ++ " with " ++ t
  errArity name = "Wrong number of arguments for " ++ name

  -- | For objects that can be overloaded by a string
  strConstruct typ str =
    componentByName typ ns >>=
      \(Component c) -> if c.signature == [typ, "String"]
                  then Just (valueComponent [typ] str)
                  else Nothing


componentByName :: Type -> Namespace -> Maybe Component
componentByName typ ns = lookup typ ns

-- instance eqCode :: Eq Code where
--   eq (List_T {typ,pos,merr,arr}) (List_T {typ=typ2,pos=pos2,merr=merr2,arr=arr2}) =
--     typ == typ2 && pos == pos2 && merr == merr2 && arr == arr2
--   eq (Atom_T {typ,pos,atom,ecomp}) (Atom_T {typ=typ2,pos=pos2,atom=atom2,ecomp=ecomp2}) =
--     typ == typ2 && pos == pos2 && atom == atom2 && ecomp == ecomp2
--   eq (List_T _) (Atom_T _) = false
--   eq (Atom_T _) (List_T _) = false

-- instance showCode :: Show Code where
--   show (List_T {typ,pos,merr,arr}) =
--     "List_T {" <> typ <> "," <> show pos <> "," <> show merr <> "," <> show arr <> "}"
--   show (Atom_T {typ,pos,atom,ecomp}) =
--     "Atom_T {" <> typ <> "," <> show pos <> "," <> show atom <> "," <> show ecomp <> "}"
