module Language.Verne.Parser
  ( parse
  ) where

import Control.Alt
import Control.Apply

import Data.Array (last)
import Data.List (List(..), fromList)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)

import Prelude

import Text.Parsing.StringParser hiding (Pos(..))
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

import Language.Verne.Types


getPos :: Parser Int
getPos = Parser (\(s@{ pos = pos }) _ sc -> sc pos s)


parseArgs :: Parser (Array (LISP Pos Atom))
parseArgs = fix $ \_ -> fromList <$> many (parseArg <* skipSpaces)


-- | this thing breaks the rules of not skipping spaces after itself.
parseArg :: Parser (LISP Pos Atom)
parseArg = codePos (parseAtom <|> parseParens)
  where
  parseParens = fix $ \_ -> do
    args <- char '(' *> skipSpaces *> parseArgs <* optional (char ')')
    pure $ flip LIST args
  parseAtom = do
    atom <- parseStr <|> parseName
    pure $ flip ATOM atom


parseName :: Parser Atom
parseName = do
  a <- lowerCaseChar
  rest <- many myAlphaNum
  pure $ Name $ fromCharArray $ fromList $ Cons a rest
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


parse :: String -> ParseResult (LISP Pos Atom)
parse input =
    unParser parseCode {str: input, pos: 0} onErr checkSuccess
  where
    onErr pos (ParseError err) = Failure pos err
    onErr pos EndOfInput       = Failure pos "EndOfInput"
    parseCode = skipSpaces *> codePos (flip LIST <$> parseArgs)


checkSuccess :: LISP Pos Atom -> PosString -> ParseResult (LISP Pos Atom)
checkSuccess a _ = dive a
  where
    dive (LIST _ arr) = case last arr of
                            Nothing -> Success a
                            Just lisp -> dive lisp
    dive (ATOM _ (Catch _)) = Partial a
    dive (ATOM _ _        ) = Success a
