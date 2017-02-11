{-# LANGUAGE ExistentialQuantification #-}
module Common.ParserT where

import Protolude

import qualified Common.Stream as S
import qualified Data.Text as T
import           Unsafe (unsafeFromJust)
import           Utils ((<<))

data ParserState s = ParserState { stream :: s }

newtype ParserT s e m a = ParserT
  { runParserT :: StateT (ParserState s) (ExceptT e m) a }

instance Functor m => Functor (ParserT s e m) where
  fmap f (ParserT s) = ParserT $ fmap f s

instance Monad m => Applicative (ParserT s e m) where
  pure = ParserT . pure
  (ParserT f) <*> (ParserT a) = ParserT $ f <*> a

instance Monad m => Monad (ParserT s e m) where
  (ParserT m) >>= f = ParserT $ m >>= runParserT . f

preview :: (Monad m, S.Stream s a) => ParserT s e m (Maybe a)
preview = ParserT $ do
  ParserState stream <- get
  return $ fst <$> S.read stream

consume :: (Monad m, S.Stream s a) => ParserT s e m ()
consume = ParserT $ do
  ParserState stream <- get
  case S.read stream of
    Nothing -> return ()
    Just (_, s) -> put $ ParserState s

next :: (Monad m, S.Stream s a) => ParserT s e m a
next = unsafeFromJust <$> preview << consume

execParserT :: (Monad m, S.Stream s a) => ParserT s e m x -> s -> x
execParserT parser s = undefined
