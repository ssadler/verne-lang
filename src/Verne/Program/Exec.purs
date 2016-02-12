module Verne.Program.Exec
  ( execSync
  ) where

import Data.Either

import Prelude

import Verne.Types


execSync :: Code -> Program (Either Unit Component)
execSync code = pure (Right nullComponent)
-- execSync (Atom {component=(Right c)}) = execSync' c
-- execSync (List {error=Nothing, arr}) =
--   case uncons arr of
