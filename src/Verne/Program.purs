module Language.Verne.Program
  ( Node(..)
  , Program(..)
  ) where

import Control.Monad.State
import Control.Monad.Reader.Trans


import qualified Data.Array as A
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.StrMap (StrMap(..), empty, singleton, union)
import Data.Traversable
import Data.Tuple

import Language.Verne.Namespace (Namespace(..))
import Language.Verne.TypeChecker
import Language.Verne.Types
import Language.Verne.Utils
import Prelude

{-

The Program is an exection manager for a Code tree.

We don't bother making a heirarchy of typed objects, just shove them into a hashmap and load the DAG as fully as possible. We may even already have some of the computation results cached in our LRU. Components can set attributes like whether or not they think they should be cached.

TODO: Eventually bring the parsing into this as well. Program is our God class now! Yay!

TODO: Figure out a way to get reliable shared version hashes for components. For now: version numbers are internal.

TODO: Purescript module responsible for creating / validating components.
-}

type NodeId = String

newtype Node = Node {comp::Component, inputs::Array String}

instance hashNode :: Hashable Node where
  hash (Node {comp=Component c,inputs}) = hashMany ([c.id] ++ inputs)

-- | A program is a computation cache
-- | An AST is a source component.
-- | ASTs may link to each other but may also be fully self contained.
-- | Being self contained means that an AST may hold it's source code too.
--
newtype Program = Program
  { nodes :: StrMap Node
  }

instance semigroupProgram :: Semigroup Program where
  append (Program p1) (Program p2) = Program { nodes: union p1.nodes p2.nodes }

instance monoidProgram :: Monoid Program where
  mempty = Program {nodes:empty}


fromLISP :: LISP_T -> Program -> Tuple NodeId Program
fromLISP lisp st = runState (fromLISP' lisp) st


fromLISP' :: LISP_T -> State Program NodeId
fromLISP' (LIST_T {typ,pos,arr,merr=Nothing}) =
  case A.uncons arr of
    Just {head=(ATOM_T {ecomp=Right comp}),tail} -> do
      inputs <- traverse fromLISP' tail
      let node = Node {comp,inputs}
          nid = hash node
      modify (\p -> p <> Program {nodes:singleton nid node})
      pure nid

fromLISP' (ATOM_T {typ,pos,atom,ecomp=Right comp}) = do
  let node = Node {comp,inputs:mempty}
      nid = hash node
  modify (\p -> p <> Program {nodes:singleton nid node})
  pure nid



-- | Build program from AST
--
type Builder = ReaderT Namespace (State Program)

fromAST :: AST -> Type -> Namespace -> Tuple NodeId Program
fromAST ast typ ns = runState (runReaderT (fromAST' ast typ) ns) mempty

fromAST' :: AST -> Type -> Builder NodeId
fromAST' ast typ = pure "a"
