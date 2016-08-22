module Verne.Data.Code
  ( Code(..)
  , Executable(..)
  , Syntax(..)
  , codeErrors
  , getCompletion
  , showCodeError
  , toExecutable
  ) where

import Control.Monad.Except

import Data.Array
import Data.Either
import Data.Maybe
import Data.String (joinWith)
import Data.Traversable

import Prelude

import Verne.Data.Type
import Verne.Data.Part

-- | IR Concept
-- 
-- Having many fixed intermediary representations is a burden
-- because the cost of experimentation is high and the scope
-- of each transformation is unclear.
-- 
-- Instead, have a single IR with constructors to support or
-- annotate every case.
-- 
-- Modules that implement transformations wrap their outputs
-- in types that specify which IR constructors may be
-- present.
--
-- TODO: Draw diagrams of these transitions and what the resulting
-- configurations look like, ie, what can contain what.
--
-- This style is flexible but it is working around the type system
-- to an extent, because much of the information about a value is
-- encoded in an opaque type tree. Would be interesting to see if
-- there's a way around this that does not involve type classes.

-- | Code Tree
-- | This could actually be merged with the Syntax Tree.
data Code = Code Code (Array Code)
          | Atom Part
          | Undefined String Type
          | NeedsArgument Type
          | TooManyArguments (Array Syntax) Code
          | TypeError Type Type
          | Posc Int Int Code

newtype Executable = Executable Code

toExecutable :: Code -> Either Error Executable
toExecutable code = runExcept (Executable <$> go code)
  where
  go :: Code -> Except Error Code
  go a@(Atom part) = pure a
  go (Code constructor args) =
    Code <$> go constructor <*> traverse go args
  go (Posc a b code) = Posc a b <$> go code
  go code = except $ Left (showCodeError code)

type Error = String

codeErrors :: Code -> Array Code
codeErrors (Posc a b (Atom _)) = []
codeErrors (Posc a b (Code head args)) =
  codeErrors head ++ (args >>= codeErrors)
codeErrors c@(Posc a b code) = [c]

showCodeError :: Code -> String
showCodeError (Undefined str _) = "name '" ++ str ++ "' is undefined"
showCodeError (TypeError t1 t2) =
  "expecting type " ++ show t1 ++ " but found " ++ show t2
showCodeError (Posc a b code) =
  "at " ++ show a ++ ":" ++ show b ++ ": " ++ showCodeError code

getCompletion :: Int -> Code -> Maybe Code
getCompletion caret = go
  where
  go :: Code -> Maybe Code
  go c@(Posc a b (Code h args)) =
    inside a b $ head (mapMaybe go (h:args))
  go c@(Posc a b (Atom (Part {autocomplete:Just _}))) =
    inside a b $ Just c
  go c@(Posc a b (Undefined _ _)) =
    inside a b $ Just c
  go c@(Posc a b (NeedsArgument _)) =
    inside a b $ Just c
  go _ = Nothing
  inside a b act =
    if caret >= a && caret <= b then act else Nothing

-- | Syntax Tree
data Syntax = Syntax Syntax (Array Syntax)
            | Name String
            | Str String
            | Posi Int Int Syntax

instance showSyntax :: Show Syntax where
  show (Syntax func args) =
    "(Syntax " ++ show func ++ " " ++ joinWith " " (show <$> args) ++ ")"
  show (Name name) = name
  show (Str s) = "\"" ++ s ++ "\""
  show (Posi a b syn) = joinWith " " [show a, show b, show syn]
