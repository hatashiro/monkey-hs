module Lexer where

import Protolude

import qualified Data.Text as T
import           Lexer.Token

data LexerState = LexerState { left :: Text, done :: Text }

newtype LexerT m a = LexerT { unLexerT :: StateT LexerState m a }

instance Functor m => Functor (LexerT m) where
  fmap f (LexerT s) = LexerT $ fmap f s

instance Monad m => Applicative (LexerT m) where
  pure = LexerT . pure
  (LexerT f) <*> (LexerT a) = LexerT $ f <*> a

instance Monad m => Monad (LexerT m) where
  return = pure
  (LexerT m) >>= f = LexerT $ m >>= unLexerT . f

type Lexer = LexerT Identity

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

runLexer :: Text -> Lexer a -> a
runLexer text lexer = fst . runIdentity $
  runStateT (unLexerT lexer) $ LexerState text ""

lex :: Text -> [Token]
lex text = runLexer text go
  where
  go :: Lexer [Token]
  go = do
    c <- preview
    case c of
      Just '=' -> consume >> (Assign:) <$> go
      Just ';' -> consume >> (SemiColon:) <$> go
      Just '(' -> consume >> (LParen:) <$> go
      Just ')' -> consume >> (RParen:) <$> go
      Just ',' -> consume >> (Comma:) <$> go
      Just '+' -> consume >> (Plus:) <$> go
      Just '{' -> consume >> (LBrace:) <$> go
      Just '}' -> consume >> (RBrace:) <$> go
      Just _ -> undefined
      Nothing -> return [EOF]
