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
next = LexerT $ do
  LexerState left done <- get
  if T.length left == 0
  then
    return Nothing
  else do
    let c = T.head left
    put $ LexerState (T.tail left) (T.snoc done c)
    return $ Just c

runLexer :: Text -> Lexer a -> a
runLexer text lexer = fst . runIdentity $
  runStateT (unLexerT lexer) $ LexerState text ""

lex :: Text -> [Token]
lex text = runLexer text go
  where
  go :: Lexer [Token]
  go = do
    maybeC <- next
    case maybeC of
      Just '=' -> (Assign:) <$> go
      Just ';' -> (SemiColon:) <$> go
      Just '(' -> (LParen:) <$> go
      Just ')' -> (RParen:) <$> go
      Just ',' -> (Comma:) <$> go
      Just '+' -> (Plus:) <$> go
      Just '{' -> (LBrace:) <$> go
      Just '}' -> (RBrace:) <$> go
      Just _ -> undefined
      Nothing -> return [EOF]
