module Verne.Program.Completions
  ( Completion(..)
  , CompletionResult(..)
  , getCompletion
  ) where

import Control.Alt
import Control.Apply
import Control.Monad
import Control.Monad.Except
import Control.Monad.Except.Trans

import Data.Array ((!!), drop, head, filter, length, tail)
import Data.Either
import Data.Foreign
import Data.Identity (Identity(..))
import Data.Maybe
import Data.String (indexOf, toLower)
import Data.Traversable

import Verne.Types
import Verne.Program.Exec

import Debug.Trace

import Prelude


-- | Autocomplete computation
type Autocomplete a = ExceptT CompletionResult Program a

-- | Completion result
newtype CompletionResult = Result {completion::Completion,pos::Pos}
data Completion = NameCompletions (Array Component)
                | ComponentAutocomplete Component (Array Component)
                | ShowError Error

type Context = {code::Code,caret::Int}

-- | Walk the tree until the node where the caret is is found
-- | then autocomplete based on the available information.
dive :: Context -> Autocomplete Unit
dive r = let pos = case getPos r.code of Pos p -> p
          in when (r.caret >= pos.a && r.caret <= pos.b) (dive' r)

dive' :: Context -> Autocomplete Unit
dive' r = case r.code of
    List {typ,arr} -> do
        -- When the list is empty, get completions for top type
        when (length arr == 0) (lookupName r "" typ)
        
        -- Check the arr for inner completions
        traverse (\l -> dive (r {code=l})) arr

        -- Get completions for next type
        completeNextArg r arr

    Atom {typ,atom,component=(Right component)} -> do
        -- Has component autocomplete?
        --componentAutocomplete r component

        -- Component is complete.
        case atom of (Name name) -> lookupName r name typ
                     _           -> return unit

    Atom {typ,pos,atom,component=(Left err)} -> do
        -- Show name completions
        case atom of (Name name) -> lookupName r name typ
                     _           -> return unit

        -- Show error as a fallback
        complete (result pos (ShowError err))

onLeft :: forall a b. (a -> a) -> Except a b -> Except a b
onLeft f a = except $ case runExcept a of
                          Left a -> Left (f a)
                          b      -> b

-- | Provide completions for next argument
completeNextArg :: Context -> Array Code -> Autocomplete Unit
completeNextArg r arr = do
    let offset = length arr
    case head arr of
      Just (Atom {component=(Right (Component comp))}) -> do
            -- First try autocomplete property of the head component
            componentAutocomplete r (Component comp) (drop 1 arr)
            -- Then try the long way
            -- case comp.signature !! offset of
            --      Nothing -> pure unit
            --      Just typ -> do
            --          let caretPos = Pos {a:r.caret,b:r.caret}
            --          onLeft (\(Result cr) -> Result (cr {pos=caretPos})) $ do
            --              completionWidget r typ
            --              lookupName r "" typ
      _ -> pure unit

-- completionWidget :: Context -> String -> Autocomplete Unit
-- completionWidget r name =
--     maybe (return unit) (componentAutocomplete r) $ 
--         componentByName name r.ns

componentAutocomplete :: Context -> Component -> Array Code -> Autocomplete Unit
componentAutocomplete r c@(Component {autocomplete}) arr =
    case autocomplete of
         Nothing -> pure unit
         Just _ -> do
           res <- lift (sequence <$> traverse execSync arr)
           case res of
             Left _ -> pure unit
             Right args -> complete (result (getPos r.code)
                                    (ComponentAutocomplete c args))

componentsByTypeHead :: Type -> Autocomplete (Array Component)
componentsByTypeHead typ = pure []

lookupName :: Context -> Prefix -> Type -> Autocomplete Unit
lookupName r prefix typ = do
  comps <- componentsByTypeHead typ
  let lpref = toLower prefix
      hasPrefix (Component c) = indexOf lpref (toLower c.name) == Just 0
      matches = filter hasPrefix comps
  complete $ result (getPos r.code) (NameCompletions matches)

--
-- | Stuff to do with the Autocomplete computation type

getCompletion :: Int -> Code -> Program (Maybe CompletionResult)
getCompletion caret code = either Just (\unit -> Nothing) <$>
    runExceptT (dive {code,caret})
 
complete :: CompletionResult -> Autocomplete Unit
complete = ExceptT <<< pure <<< Left

result :: Pos -> Completion -> CompletionResult
result p c = Result {pos:p,completion:c}

getPos :: Code -> Pos
getPos (List {pos=p}) = p
getPos (Atom {pos=p}) = p

type Prefix = String
