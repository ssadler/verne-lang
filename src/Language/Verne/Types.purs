module Language.Verne.Types
  ( LISP(..)
  , Pos(..)
  , Atom(..)
  , ParseResult(..)
  ) where


import Data.Generic

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))


-- | Core syntax tree container
--
data LISP a b = LIST a (Array (LISP a b)) | ATOM a b

derive instance genericLISP :: (Generic a, Generic b) => Generic (LISP a b)

instance eqLISP :: (Eq a, Eq b, Generic a, Generic b) => Eq (LISP a b)
  where eq = gEq

instance showLISP :: (Show a, Show b, Generic a, Generic b) => Show (LISP a b)
  where show = gShow


-- | Byte offset specifier
--
data Pos = Pos Int Int

derive instance genericPos :: Generic Pos
instance eqPos :: Eq Pos where eq = gEq
instance showPos :: Show Pos where show = gShow


-- | Data definition
--
data Atom = Name String | Str String | Catch ParseError

derive instance genericAtom :: Generic Atom
instance eqAtom :: Eq Atom where eq = gEq
instance showAtom :: Show Atom where show = gShow

data ParseResult a = Success a | Partial a | Failure Int String

derive instance genericParseResult :: (Generic a) => Generic (ParseResult a)
instance eqParseResult :: (Eq a, Generic a) => Eq (ParseResult a) where eq = gEq
instance showParseResult :: (Show a, Generic a) => Show (ParseResult a) where show = gShow
