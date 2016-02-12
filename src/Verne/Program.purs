module Verne.Program
  ( codeCompletion
  , execCode
  , parseCode
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

import Verne.Types
import Verne.Utils
import Verne.Program.Exec
import Verne.Program.Compiler
import Verne.Program.Completions
import Verne.Program.Parser
import Prelude

parseCode :: Namespace -> Type -> String -> Either ParseFail Code
parseCode ns typ str = compile ns typ <$> parse str

execCode :: ProgramState -> Code -> ProgramState
execCode ps code = execState (pure code) ps

codeCompletion :: ProgramState -> Int -> Code -> Maybe CompletionResult
codeCompletion ps caret code = evalState (getCompletion caret code) ps

-- | Fuck. Program needs to be called interpreter, because if we want to support
-- | language extensions, the parser needs access to a state store. Which is kinda
-- | has already, i suppose, in the namespace... Hmm. Should language extensions and
-- | runtime concerns mingle? Or are all language extensions just desugared by the
-- | compiler? Also, "program" suggests it contains the code, yet, it does not.
