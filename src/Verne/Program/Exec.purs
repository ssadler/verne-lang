module Verne.Program.Exec
  ( execSync
  ) where

import Data.Either

import Prelude

import Verne.Types


execSync :: Code -> Program (Either Unit Component)
execSync code = pure (Right nullComponent)
