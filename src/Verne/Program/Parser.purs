module Verne.Program.Parser
  ( module SP
  , ParseFail(..)
  , parse
  ) where

import Control.Alt
import Control.Apply

import Data.Either
import Data.List (List(..), fromList)
import Data.String (fromCharArray)

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String
import qualified Text.Parsing.StringParser (ParseError(..)) as SP

import Verne.Types.Program

type ParseFail = {pos::Int, error::ParseError}

-- | Maxpos is a number that's always bigger than another number.
-- In this case the length of a code expression.  Used to mean "infinity".
maxpos :: Int
maxpos = 1000000

getPos :: Parser Int
getPos = Parser (\(s@{ pos = pos }) _ sc -> sc pos s)

parseArgs :: Parser (Array AST)
parseArgs = fix $ \_ -> fromList <$> many (parseArg <* skipSpaces)

-- | this thing breaks the rules of not skipping spaces after itself.
parseArg :: Parser AST
parseArg = parseStr <|> parseName <|> parseParens
  where
  parseParens = fix $ \_ -> do
    a <- getPos
    args <- char '(' *> skipSpaces *> parseArgs
    b <- (eof *> pure maxpos) <|> (char ')' *> getPos)
    pure $ SList {pos:Pos {a,b},arr:args}

parseName :: Parser AST
parseName = codePos $ ta <$> do
  a <- lowerCaseChar
  rest <- many myAlphaNum
  pure $ Name $ fromCharArray $ fromList $ Cons a rest
  where
  ta a b = SAtom {atom:a,pos:b}
  myAlphaNum = satisfy $ \c -> c >= 'a' && c <= 'z'
                || c >= 'A' && c <= 'Z'
                || c >= '0' && c <= '9' 

parseStr :: Parser AST
parseStr = do
  a <- getPos
  char '"'
  str <- many $ satisfy $ (not <<< (=='"'))
  b <- (eof *> pure maxpos) <|> (char '"' *> getPos)
  let pos = Pos {a,b}
  pure $ SAtom {pos,atom:Str (fromCharArray (fromList str))}

codePos :: Parser (Pos -> AST) -> Parser AST
codePos p = do
  a <- getPos
  f <- p
  b <- getPos
  pure $ f $ Pos {a,b}

parse :: String -> Either {pos::Int, error::ParseError} AST
parse input = unParser parseCode {str: input, pos: 0} onErr onSuccess
  where
  onSuccess ast _ = Right ast
  onErr pos error = Left {pos,error}
  parseCode = SList <$> ({pos:_,arr:_} <$> thePos <*> parseArgs)
  thePos = (\a b -> Pos {a,b}) <$> getPos <*> pure maxpos

