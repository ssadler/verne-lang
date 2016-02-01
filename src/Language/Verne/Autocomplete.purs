module Autocomplete where

import Control.Alt

import Data.Array ((!!), head, length, last, uncons)
import qualified Data.Array.Unsafe as U
import Data.Either
import Data.Foldable (foldl)
import Data.Foreign
import Data.Maybe

import Language.Verne.Namespace
import Language.Verne.TypeChecker
import Language.Verne.Types

import Prelude

data Result = CompletionsForType Type
            | ComponentAutocomplete Foreign
            | ShowError Error

getCompletion :: Int -> LISP_T Atom -> Maybe Result
getCompletion caret (LIST_T {pos=Pos a b, typ, arr}) =
    if caret >= a && caret <= b
       then find (getCompletion caret) arr <|> nextArg typ arr
       else Nothing
getCompletion caret (ATOM_T {pos=Pos a b,atom,typ,ecomp}) =
    if caret >= a && caret <= b
       then case ecomp of
         Right (Component comp) -> ComponentAutocomplete <$> comp.autocomplete
         Left err -> Just (ShowError err)
       else Nothing

nextArg :: Type -> Array (LISP_T Atom) -> Maybe Result
nextArg typ arr =
    let f (ATOM_T {ecomp}) =
        case ecomp of
             Right (Component comp) -> comp.signature !! length arr
             Left err -> Nothing
     in CompletionsForType <$> ((head arr >>= f) <|> pure typ)

find :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
find f arr =
    uncons arr >>=
        \{head=h, tail=t} -> case f h of Nothing -> find f t
                                         a       -> a

