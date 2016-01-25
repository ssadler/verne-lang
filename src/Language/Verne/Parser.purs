module Language.Verne.Parser
  ( parse
  ) where

import Control.Alt
import Control.Apply

import Data.Array ((:))
import Data.Either
import Data.List (fromList)
import Data.String (fromCharArray)

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

import Language.Verne.Types


getPos :: Parser Int
getPos = Parser (\(s@{ pos = pos }) _ sc -> sc pos s)


parseArgs :: Parser (Array (LISP Pos Atom))
parseArgs = fix $ \_ -> do
  let out = fromList <$> sepBy1 parseArg (char ' ' *> skipSpaces)
  out <* skipSpaces


parseArg :: Parser (LISP Pos Atom)
parseArg = codePos (parseParens <|> parseAtom)
  where
  parseParens = fix $ \_ -> do
    args <- between (char '(') (char ')') parseArgs
    pure $ flip LIST args
  parseAtom = do
    atom <- parseStr <|> parseName
    pure $ flip ATOM atom


parseName :: Parser Atom
parseName = do
  a <- lowerCaseChar
  rest <- many myAlphaNum
  pure $ Name $ fromCharArray $ a : fromList rest
  where
  myAlphaNum = satisfy $ \c -> c >= 'a' && c <= 'z'
                || c >= 'A' && c <= 'Z'
                || c >= '0' && c <= '9' 


parseStr :: Parser Atom
parseStr =
  let str = many $ satisfy $ (not <<< (=='"'))
      lit = between (char '"') (char '"') str
  in  Str <$> fromCharArray <<< fromList <$> lit


codePos :: forall b. Parser (Pos -> LISP Pos b) -> Parser (LISP Pos b)
codePos p = do
  a <- getPos
  f <- p
  b <- getPos
  pure $ f $ Pos a b


parse :: String -> Either Error (LISP Pos Atom)
parse command = unParser parseCode {str: command, pos: 0} onErr onSucc
  where onErr pos (ParseError err) = Left (Error {pos: pos, err: err})
        onSucc result _ = Right result
        parseCode = skipSpaces *> codePos (flip LIST <$> parseArgs)

