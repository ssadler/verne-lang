module Language.Verne.Types
  ( LISP(..)
  , Atom(..)
  , Component(..)
  , Error(..)
  , ParseResult(..)
  , Pos(..)
  , Type(..)
  ) where


import Data.Generic
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.NullOrUndefined
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

newtype Component = Component
    { name :: String
    , signature :: Array Type
    , autocomplete :: Maybe Foreign
    , exec :: Foreign
    }

instance componentIsForeign :: IsForeign Component where
    read fo = Component <$> ({name:_, signature:_, autocomplete:_, exec:_}
                        <$> readProp "name" fo
                        <*> readProp "signature" fo
                        <*> (runNullOrUndefined <$> readProp "autocomplete" fo)
                        <*> readProp "exec" fo
                            )

instance showComponent :: Show Component where
    show (Component {name,signature,autocomplete}) =
        let f = (\_ -> "f()") <$> autocomplete
         in "Component {" <> name <> "," <> show signature <> "," <> show f <> "}"

instance eqComponent :: Eq Component where
    eq (Component {name,signature,autocomplete,exec})
       (Component {name=n2,signature=s2,autocomplete=a2,exec=e2}) =
           name == n2 && signature == s2 && isSame autocomplete a2 && isSame exec e2


-- | Core language types
--
type Type = String
type Error = String
