module Language.Verne.Completions
  ( Completion(..)
  , CompletionResult(..)
  , getCompletion
  ) where

import Control.Alt
import Control.Apply
import Control.Monad
import Control.Monad.Except

import Data.Array ((!!), head, filter, length)
import Data.Either
import Data.Foreign
import Data.Identity (Identity(..))
import Data.Maybe
import Data.String (indexOf, toLower)
import Data.Traversable

import Language.Verne.Namespace
import Language.Verne.TypeChecker
import Language.Verne.Types

import Debug.Trace

import Prelude

-- | This module takes care of getting autocomplete data from a typed expression.

-- | Autocomplete computation
type Autocomplete a = Except CompletionResult a

-- | Completion result
newtype CompletionResult = Result {completion::Completion,pos::Pos}
data Completion = NameCompletions (Array Component)
                | ComponentAutocomplete Component
                | ShowError Error

type Context = {lisp::LISP_T Atom,ns::Namespace,caret::Int}

-- | Walk the tree until the node where the caret is is found
-- | then autocomplete based on the available information.
findCompletion :: Context -> Autocomplete Unit
findCompletion r = let pos = case getPos r.lisp of Pos p -> p
                    in when (r.caret >= pos.a && r.caret <= pos.b)
                            (findCompletion' r)

findCompletion' :: Context -> Autocomplete Unit
findCompletion' r = case r.lisp of
    LIST_T {typ,arr} -> do
        -- When the list is empty, get completions for top type
        when (length arr == 0) (findCompletions r "" typ)
        
        -- Check the arr for inner completions
        traverse (\l -> findCompletion (r {lisp=l})) arr

        -- Get completions for next type
        completeNextArg r arr

    ATOM_T {typ,atom,ecomp=(Right component)} -> do
        -- Has component autocomplete?
        componentAutocomplete r component

        -- Component is complete.
        case atom of (Name name) -> findCompletions r name typ
                     _           -> return unit

    ATOM_T {typ,pos,atom,ecomp=(Left err)} -> do
        -- Show name completions
        case atom of (Name name) -> findCompletions r name typ
                     _           -> return unit

        -- Show error as a fallback
        complete (result pos (ShowError err))

onLeft :: forall a b. (a -> a) -> Except a b -> Except a b
onLeft f a = except $ case runExcept a of
                          Left a -> Left (f a)
                          b      -> b

-- | Provide completions for next argument
completeNextArg :: Context -> Array (LISP_T Atom) -> Autocomplete Unit
completeNextArg r arr = do
    let offset = length arr
    case head arr of 
        Nothing -> pure unit
        Just (ATOM_T {ecomp=(Right (Component comp))}) ->
            case comp.signature !! offset of
                 Nothing -> pure unit
                 Just typ -> do
                     let caretPos = Pos {a:r.caret,b:r.caret}
                     onLeft (\(Result cr) -> Result (cr {pos=caretPos})) $ do
                         completionWidget r typ
                         findCompletions r "" typ

completionWidget :: Context -> String -> Autocomplete Unit
completionWidget r name =
    maybe (return unit) (componentAutocomplete r) $ 
        componentByName name r.ns

componentAutocomplete :: Context -> Component -> Autocomplete Unit
componentAutocomplete r c@(Component {autocomplete}) =
    case autocomplete of
         Nothing -> pure unit
         Just _ -> complete (result (getPos r.lisp) (ComponentAutocomplete c))

findCompletions :: Context -> Prefix -> Type -> Autocomplete Unit
findCompletions r prefix typ =
    let comps = componentsByTypeHead typ r.ns
        lpref = toLower prefix
        hasPrefix (Component c) = indexOf lpref (toLower c.name) == Just 0
        matches = filter hasPrefix comps
    in complete $ result (getPos r.lisp) (NameCompletions matches)

--
-- | Stuff to do with the Autocomplete computation type

getCompletion :: Namespace -> Int -> LISP_T Atom -> Maybe CompletionResult
getCompletion ns caret lisp = either Just (\_ -> Nothing) $
    runExcept (findCompletion {lisp,ns,caret})
 
complete :: CompletionResult -> Autocomplete Unit
complete = except <<< Left

result :: Pos -> Completion -> CompletionResult
result p c = Result {pos:p,completion:c}

getPos :: LISP_T Atom -> Pos
getPos (LIST_T {pos=p}) = p
getPos (ATOM_T {pos=p}) = p

type Prefix = String
