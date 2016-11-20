module Verne.Utils.Parsing where

import Control.Alt
import Control.Alternative
import Control.Plus

import Data.Array ((..))
import Data.Char (toCharCode)
import Data.Either
import Data.Foldable (foldMap, elem, notElem)
import Data.Maybe
import Data.List (List(..))
import Data.String as S

import Prelude

-- | A poition in an input string.
type Pos = Int

-- | Strings are represented as a string with an index from the
-- | start of the string.
-- | 
-- | `{ str: s, pos: n }` is interpreted as the substring of `s`
-- | starting at index n.
-- | 
-- | This allows us to avoid repeatedly finding substrings
-- | every time we match a character.
type PosString = { str :: String, pos :: Pos }

type Success a b = (a -> PosString -> b)

-- | The type of parsing errors.
data ParseError = ParseError String

instance showParseError :: Show ParseError where
  show (ParseError msg) = msg

instance eqParseError :: Eq ParseError where
  eq (ParseError msg) (ParseError msg2) = msg == msg2

-- | A parser is represented as a function which takes a pair of
-- | continuations for failure and success.
data Parser a = Parser (forall r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r)

-- | Run a parser by providing success and failure continuations.
unParser :: forall a r. Parser a -> PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r
unParser (Parser p) = p

-- | Run a parser for an input string, returning either an error or a result.
runParser :: forall a. Parser a -> String -> Either ParseError a
runParser p s = unParser p { str: s, pos: 0 } (\_ err -> Left err) (\a _ -> Right a)

instance functorParser :: Functor Parser where
  map f p = Parser (\s fc sc ->
    unParser p s fc (\a s' -> sc (f a) s'))

instance applyParser :: Apply Parser where
  apply (Parser p1) (Parser p2) = Parser \s fc sc ->
    p1 s fc \f s' ->
      p2 s' fc \a -> sc (f a)

instance applicativeParser :: Applicative Parser where
  pure a = Parser (\s _ sc -> sc a s)

instance altParser :: Alt Parser where
  alt p1 p2 = Parser (\s fc sc ->
    unParser p1 s (\_ _ -> unParser p2 s fc sc) sc)

instance plusParser :: Plus Parser where
  empty = fail "No alternative"

instance alternativeParser :: Alternative Parser

instance bindParser :: Bind Parser where
  bind p f = Parser (\s fc sc ->
    unParser p s fc (\a s' ->
      unParser (f a) s' fc sc))

instance monadParser :: Monad Parser

-- | Fail with the specified message.
fail :: forall a. String -> Parser a
fail msg = Parser pp
  where
    pp :: forall a r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r
    pp { pos : pos } fc _ = fc pos (ParseError msg)

-- | Match zero or more times.
many :: forall a. Parser a -> Parser (List a)
many p = many1 p <|> pure Nil

-- | Match one or more times.
many1 :: forall a. Parser a -> Parser (List a)
many1 p = do
  a <- p
  as <- many p
  pure (Cons a as)

-- | Provide an error message in case of failure.
orError :: forall a. Parser a -> String -> Parser a
orError p msg = p <|> fail msg
infix 5 orError as <?>

-- | Take the fixed point of a parser function. This function is sometimes useful when building recursive parsers.
fix :: forall a. (Parser a -> Parser a) -> Parser a
fix f = Parser (\str fc sc -> unParser (f (fix f)) str fc sc)

-- | In case of error, the default behavior is to backtrack if no input was consumed.
-- |
-- | `try p` backtracks even if input was consumed.
try :: forall a. Parser a -> Parser a
try p = Parser pp
  where
    pp :: forall r. PosString -> (Pos -> ParseError -> r) -> (a -> PosString -> r) -> r
    pp s fc = (unParser p) s (\_ -> fc (getPos s))
    getPos {pos:pos} = pos

-- | Match a character satisfying the given predicate.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy f = try do
  c <- anyChar
  if f c
     then pure c
     else fail $ "Character " <> S.singleton c <> " did not satisfy predicate"

-- | Match the specified character.
char :: Char -> Parser Char
char c = satisfy (\s -> s == c) <?> ("Could not match character " <> S.singleton c)

-- | Match the end of the file.
eof :: Parser Unit
eof = Parser pp
  where
    pp :: forall r. PosString -> (Pos -> ParseError -> r) -> (Unit -> PosString -> r) -> r
    pp s fc sc = case s of
      { str : str, pos : i } | i < S.length str -> fc i (ParseError "Expected EOF")
      _ -> sc unit s

-- | Match any character.
anyChar :: Parser Char
anyChar = Parser \s fc sc -> case s of
  { str : str, pos : i } -> case S.charAt i str of
    Just chr -> sc chr { str: str, pos: i + 1 }
    Nothing -> fc i (ParseError "Unexpected EOF")

-- | Match any digit.
anyDigit :: Parser Char
anyDigit = try do
  c <- anyChar
  if c >= '0' && c <= '9'
     then pure c
     else fail $ "Character " <> S.singleton c <> " is not a digit"

-- | Match many whitespace characters.
whiteSpace :: Parser String
whiteSpace = do
  cs <- many (satisfy \ c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap S.singleton cs)

-- | Skip many whitespace characters.
skipSpaces :: Parser Unit
skipSpaces = void whiteSpace

-- | Match any lower case character.
lowerCaseChar :: Parser Char
lowerCaseChar = do
  c <- anyChar
  if toCharCode c `elem` (97 .. 122)
     then pure c
     else fail $ "Expected a lower case character but found '" <> S.singleton c <> "'"

-- | Match any upper case character.
upperCaseChar :: Parser Char
upperCaseChar = do
  c <- anyChar
  if toCharCode c `elem` (65 .. 90)
     then pure c
     else fail $ "Expected an upper case character but found '" <> S.singleton c <> "'"

-- | Match a letter or a number.
alphaNum :: Parser Char
alphaNum = lowerCaseChar <|> upperCaseChar <|> anyDigit <?> "Expected a letter or a number"
