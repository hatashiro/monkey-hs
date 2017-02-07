module Lexer.Types where

import Protolude

data LexerState = LexerState { left :: Text, done :: Text }

initState :: Text -> LexerState
initState = flip LexerState ""

newtype LexerT m a = LexerT { unLexerT :: StateT LexerState m a }

instance Functor m => Functor (LexerT m) where
  fmap f (LexerT s) = LexerT $ fmap f s

instance Monad m => Applicative (LexerT m) where
  pure = LexerT . pure
  (LexerT f) <*> (LexerT a) = LexerT $ f <*> a

instance Monad m => Monad (LexerT m) where
  (LexerT m) >>= f = LexerT $ m >>= unLexerT . f

type Lexer = LexerT Identity
