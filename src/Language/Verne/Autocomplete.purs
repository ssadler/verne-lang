module Language.Verne.Autocomplete 
  ( Completion(..)
  , runAutocomplete
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans

import Data.Array ((!!), head, filter, length)
import Data.Either
import Data.Foreign
import Data.Identity (Identity(..))
import Data.Maybe
import Data.String (indexOf,toLower)
import Data.Traversable

import Language.Verne.Namespace
import Language.Verne.TypeChecker
import Language.Verne.Types

import Prelude

-- | This module takes care of getting autocomplete data from a typed expression.

-- | Autocomplete computation
type Autocomplete = ReaderT {ns::Namespace,caret::Int} (ExceptT Completion Identity)

-- | Completion result
data Completion = NameCompletions (Array Component)
                | ComponentAutocomplete Foreign
                | ShowError Error

-- | Walk the tree until the node where the caret is is found
-- | then autocomplete based on the available information.
getCompletion :: LISP_T Atom -> Autocomplete Unit
getCompletion lisp = ask >>=
    \r -> case getPos lisp of
               Pos a b -> if r.caret >= a && r.caret <= b
                       then getCompletion' lisp
                       else return unit
  where
    getPos (LIST_T {pos}) = pos
    getPos (ATOM_T {pos}) = pos

getCompletion' :: LISP_T Atom -> Autocomplete Unit
getCompletion' atom = case atom of
    LIST_T {typ,arr} -> do
        -- When the list is empty, get completions for top type
        when (length arr == 0) (nameCompletions "" typ)
        
        -- Check the arr for inner completions
        -- TODO: don't trust sequence
        sequence (getCompletion' <$> arr)

        -- Get completions for next type
        completeNextArg arr

    ATOM_T {typ,atom,ecomp=(Right component)} -> do
        -- Has component autocomplete?
        componentAutocomplete component

    ATOM_T {typ,atom,ecomp=(Left err)} -> do
        -- Show name completions
        case atom of (Name name) -> nameCompletions name typ
                     _           -> return unit

        -- Show error as a fallback
        complete $ ShowError err

completeNextArg :: Array (LISP_T Atom) -> Autocomplete Unit
completeNextArg arr = do
    let offset = length arr
    case head arr of 
        Just (ATOM_T {ecomp=(Right (Component comp))}) ->
            case comp.signature !! offset of
                 Just typ -> nameCompletions "" typ
                 Nothing -> pure unit
        Nothing -> pure unit

componentAutocomplete :: Component -> Autocomplete Unit
componentAutocomplete (Component {autocomplete}) = 
    maybe (pure unit) (complete <<< ComponentAutocomplete) autocomplete

nameCompletions :: Prefix -> Type -> Autocomplete Unit
nameCompletions name typ = do
    r <- ask
    let comps = componentsByTypeHead typ r.ns
        lname = toLower name
        hasPrefix (Component c) = indexOf lname (toLower c.name) == Just 0
        matches = filter hasPrefix comps
    complete $ NameCompletions matches


--
-- | Stuff to do with the Autocomplete computation type

runAutocomplete :: Namespace -> Int -> LISP_T Atom -> Maybe Completion
runAutocomplete ns caret lisp = either Just (\_ -> Nothing) $
     runExcept $ runReaderT (getCompletion lisp) {ns,caret}
 
complete :: Completion -> Autocomplete Unit
complete = lift <<< except <<< Left

type Prefix = String
