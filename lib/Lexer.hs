module Lexer where

import Protolude

import           Common.ParserT (consume, preview, next)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Lexer.Token
import           Lexer.Types
import           Utils (unsafeFromRight, isLetter, isDigit, (<||>))

lexChar :: Char -> Lexer Token
lexChar '=' = consume >> preview >>= \maybeC ->
  case maybeC of
    Just '=' -> consume $> Eq
    _ -> return Assign
lexChar ';' = consume $> SemiColon
lexChar '(' = consume $> LParen
lexChar ')' = consume $> RParen
lexChar ',' = consume $> Comma
lexChar '+' = consume $> Plus
lexChar '-' = consume $> Minus
lexChar '*' = consume $> Multiply
lexChar '/' = consume $> Divide
lexChar '!' = consume >> preview >>= \maybeC ->
  case maybeC of
    Just '=' -> consume $> NotEq
    _ -> return Not
lexChar '>' = consume $> GreaterThan
lexChar '<' = consume $> LessThan
lexChar '{' = consume $> LBrace
lexChar '}' = consume $> RBrace
lexChar c
  | isLetter c = lexIdentOrReserved
  | isDigit c = lexInteger
  | otherwise = consume $> Illegal

lexText :: (Char -> Bool) -> Lexer Text
lexText f = preview >>= \maybeC ->
  case maybeC of
    Just c ->
      if f c
      then consume >> T.cons c <$> lexText f
      else return ""
    Nothing -> return ""

lexIdentOrReserved :: Lexer Token
lexIdentOrReserved = do
  text <- T.cons <$> next <*> lexText (isLetter <||> isDigit)
  return $ case text of
    "let" -> Let
    "fn" -> Function
    "if" -> If
    "else" -> Else
    "return" -> Return
    "true" -> BoolLiteral True
    "false" -> BoolLiteral False
    _ -> Ident text

readInteger :: Text -> Integer
readInteger = fst . unsafeFromRight . decimal

lexInteger :: Lexer Token
lexInteger = IntLiteral . readInteger <$> lexText isDigit

skipWhitespaces :: Lexer ()
skipWhitespaces = preview >>= \maybeC ->
  case maybeC of
    Just ' ' -> consume >> skipWhitespaces
    Just '\t' -> consume >> skipWhitespaces
    Just '\n' -> consume >> skipWhitespaces
    Just '\r' -> consume >> skipWhitespaces
    _ -> return ()

lex :: Text -> [Token]
lex = execLexer go
  where
  go :: Lexer [Token]
  go = do
    skipWhitespaces
    c <- preview
    case c of
      Just x -> (:) <$> lexChar x <*> go
      Nothing -> return [EOF]
