module Verne.Data.Type where

import Control.Alt
import Control.Apply

import Data.Array ((:), drop, length, take, uncons)
import Data.Generic
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.String (fromCharArray)
import Data.List (List(..), fromList)

import Prelude

import Verne.Utils.Parsing

-- http://dev.stephendiehl.com/fun/006_hindley_milner.html

data Type = Type Type Type
          | TCon String
          | TNil

typeLength :: Type -> Int
typeLength (Type _ b) = 1 + typeLength b
typeLength t = 1

typeToArr :: Type -> Array Type
typeToArr (Type a b) = a : typeToArr b
typeToArr t = [t]

typeFromArr :: Array Type -> Type
typeFromArr arr = case uncons arr of
                       Just {head:a1,tail:[]} -> a1
                       Just {head:a1,tail:a2} -> Type a1 (typeFromArr a2)
                       Nothing -> TNil

typeEndsWith :: Type -> Type -> Boolean
typeEndsWith t@(Type _ t1) t2 = t == t2 || typeEndsWith t1 t2
typeEndsWith t1 t2 = t1 == t2

getNextArgument :: Type -> Type -> Maybe Type
getNextArgument expected actual =
  let expArr = typeToArr expected
      actArr = typeToArr actual
      diff = length actArr - length expArr 
  in if diff > 0 && drop diff actArr == expArr
        then Just (typeFromArr $ take 1 actArr)
        else Nothing

derive instance genericType :: Generic Type
instance eqType :: Eq Type where eq = gEq

instance typeIsForeign :: IsForeign Type where
  -- Abuses `JSONError` as a general error container,
  -- which it probably should be anyway
  -- https://github.com/purescript/purescript-foreign/issues/37
  read fo = read fo >>= (\str ->
              unParser parseType {str, pos:0}
                (\_ err -> Left $ JSONError $ show err)
                (\typ _ -> Right typ))

instance typeShow :: Show Type where
  show (TCon str) = str
  show (Type t1 t2) = show t1 ++ " -> " ++ show t2
  show (TNil) = "()"

parseType :: Parser Type
parseType = do
  skipSpaces
  con <- TCon <$> parseCon
  skipSpaces
  (Type con <$> rec) <|> (eof *> pure con)
  where
  rec = fix $ \_ -> try (char '-' *> char '>') *> parseType
  parseCon = toStr <$> (Cons <$> upperCaseChar <*> many alphaNum)
  toStr = fromCharArray <<< fromList

