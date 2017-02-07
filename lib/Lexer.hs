module Lexer where

import Protolude

import qualified Data.Text as T
import           Lexer.Token
import           Lexer.Types

next :: Lexer (Maybe Char)
next = do
  c <- preview
  consume
  return c

preview :: Lexer (Maybe Char)
preview = LexerT $ do
  LexerState left _ <- get
  if T.length left == 0
  then
    return Nothing
  else
    return . Just $ T.head left

consume :: Lexer ()
consume = LexerT $ do
  LexerState left done <- get
  if T.length left == 0
  then
    return ()
  else do
    put $ LexerState (T.tail left) (T.snoc done $ T.head left)

runLexer :: Lexer a -> Text -> a
runLexer = ((fst . runIdentity) .) . (. initState) . runStateT . unLexerT

lexChar :: Char -> Lexer Token
lexChar '=' = consume >> return Assign
lexChar ';' = consume >> return SemiColon
lexChar '(' = consume >> return LParen
lexChar ')' = consume >> return RParen
lexChar ',' = consume >> return Comma
lexChar '+' = consume >> return Plus
lexChar '{' = consume >> return LBrace
lexChar '}' = consume >> return RBrace
lexChar c
  | isLetter c = lexIdentOrReserved
  | isDigit c = lexInteger
  | otherwise = consume $> Illegal

isLetter :: Char -> Bool
isLetter = flip elem $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

isDigit :: Char -> Bool
isDigit = flip elem ['0' .. '9']

lexText :: (Char -> Bool) -> Lexer Text
lexText f = preview >>= \maybeC ->
  case maybeC of
    Just c ->
      if f c
      then consume >> T.cons c <$> lexText f
      else return ""

lexIdentOrReserved :: Lexer Token
lexIdentOrReserved = lexText isLetter >>= \text -> return $
  case text of
    "let" -> Let
    "fn" -> Function
    _ -> Ident text

lexInteger :: Lexer Token
lexInteger = IntLiteral <$> lexText isDigit

skipWhitespaces :: Lexer ()
skipWhitespaces = do
  maybeC <- preview
  case maybeC of
    Just ' ' -> consume >> skipWhitespaces
    Just '\t' -> consume >> skipWhitespaces
    Just '\n' -> consume >> skipWhitespaces
    Just '\r' -> consume >> skipWhitespaces
    _ -> return ()

lex :: Text -> [Token]
lex = runLexer go
  where
  go :: Lexer [Token]
  go = do
    skipWhitespaces
    c <- preview
    case c of
      Just x -> (:) <$> lexChar x <*> go
      Nothing -> return [EOF]
