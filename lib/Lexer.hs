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
lexChar _ = undefined

lex :: Text -> [Token]
lex = runLexer go
  where
  go :: Lexer [Token]
  go = do
    c <- preview
    case c of
      Just x -> (:) <$> lexChar x <*> go
      Nothing -> return [EOF]
