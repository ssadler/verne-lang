module Verne.Types.Program
  ( AST(..)
  , Atom(..)
  , Code(..)
  , Error(..)
  , Namespace(..)
  , Pos(..)
  , Program(..)
  , ProgramState(..)
  , Type(..)
  ) where

import Control.Monad.State
import Data.Generic
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe
import Data.Monoid
import Data.StrMap

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))

import Verne.Utils

import Verne.Types.Component
import Verne.Types.Hashable


-- | Core language types
--
type Type = String

type Error = String


-- | Core syntax tree container
--
derive instance genericAST :: Generic AST

instance eqAST :: Eq AST where eq = gEq
instance showAST :: Show AST where show = gShow


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


-- | Syntax Tree
--
data AST = SList {pos::Pos, arr::Array AST}
         | SAtom {pos::Pos, atom::Atom}


-- | Code tree (Annotated syntax tree)
--
data Code = List { pos::Pos
                 , typ::Type
                 , arr::Array Code
                 , error::Maybe Error
                 }
          | Atom { pos::Pos
                 , typ::Type
                 , atom::Atom
                 , component::Either Error Component
                 }


-- | Program monad
--
type Program = State ProgramState

newtype ProgramState = PS {namespace::Namespace}

type Namespace = StrMap Component
