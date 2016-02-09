module Language.Verne.Program
  ( Id(..)
  , Node(..)
  , Program(..)
  ) where

import Data.List (List(..))
import Data.Monoid
import Data.StrMap (StrMap(..), empty, union)

import Language.Verne.Types
import Prelude

{-

The Program is an exection manager for a Code tree.

We don't bother making a heirarchy of typed objects, just shove them into a hashmap and load the DAG as fully as possible. We may even already have some of the computation results cached in our LRU. Components can set attributes like whether or not they think they should be cached.

TODO: Eventually bring the parsing into this as well. Program is our God class now! Yay!

TODO: Figure out a way to get reliable shared version hashes for components. For now: version numbers are internal.

TODO: Purescript module responsible for creating / validating components.
-}

type Id = String

newtype Node = Node {comp::Component, inputs::List Id}

newtype Program = Program
  { nodes :: StrMap Node
  }

instance semigroupProgram :: Semigroup Program where
  append (Program p1) (Program p2) = Program {nodes: union p1.nodes p2.nodes}

instance monoidProgram :: Monoid Program where
  mempty = Program {nodes:empty}


--getNodeId :: Node -> Id

--fromLISP :: LISP_T -> Program -> Program
--fromLISP = do
