module Verne.Data.Type where

import Data.Array

-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Type = Type Type Type
          | TCon String
          | TNil

lastType :: Type -> Type
lastType (Type _ b) = last b
lastType t = t

typeLength :: Type -> Int
typeLength (Type _ b) = 1 + typeLength b
typeLength t = 1

typeToArr :: Type -> Array Type
typeToArr (Type a b) = a : typeToArr b
typeToArr t = [t]

typeFromArr :: Array Type -> Type
typeFromArr arr = case uncons arr of
                       Just {head:a1,tail:[a2]} -> Type a1 a2
                       Just {head:a1,tail:a2} -> Type a1 (typeFromArr a2)
                       Nothing -> TNil
