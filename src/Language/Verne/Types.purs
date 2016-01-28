module Language.Verne.Types
  ( LISP(..)
  , LISP_T(..)
  , Atom(..)
  , Component(..)
  , Error(..)
  , ParseResult(..)
  , Pos(..)
  , Type(..)
  ) where


import Data.Either
import Data.Generic
import Data.Foreign
import Data.Foreign.Class
import qualified Data.Map as Map
import Data.Maybe

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))

import Language.Verne.Utils


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

newtype Component = Component { name :: String, signature :: Array Type }

derive instance genericComponent :: Generic Component

instance componentIsForeign :: IsForeign Component where
    read fo = Component <$>
        ({name:_, signature:_} <$> readProp "name" fo <*> readProp "signature" fo)

type Error = String

data LISP_T a = LIST_T Type Pos (Maybe Error) (Array (LISP_T a))
              | ATOM_T Type Pos a (Either Error Component)

derive instance genericLISP_T :: (Generic a) => Generic (LISP_T a)
instance eqLISP_T :: (Generic a, Eq a) => Eq (LISP_T a) where eq = gEq
instance showLISP_T :: (Generic a, Show a) => Show (LISP_T a) where
    show = compactShow <<< gShow


-- | Core language types
--
type Type = String
