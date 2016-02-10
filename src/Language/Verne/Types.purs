module Language.Verne.Types
  ( module Com
  , module Core
  , module Has
  , AST(..)
  , Atom(..)
  , ParseResult(..)
  , Pos(..)
  ) where


import Data.Generic
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))

import Language.Verne.Types.Component as Com
import Language.Verne.Types.Core as Core
import Language.Verne.Types.Hashable as Has

import Language.Verne.Utils

-- | Core syntax tree container
--
data AST = LIST Pos (Array AST) | ATOM Pos Atom

derive instance genericAST :: Generic AST

instance eqAST :: Eq AST
  where eq = gEq

instance showAST :: Show AST
  where show = gShow


-- | Byte offset specifier
--
newtype Pos = Pos {a::Int,b::Int}

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
