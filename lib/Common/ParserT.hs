{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Common.ParserT where

import Protolude hiding (many)

import qualified Common.Stream as S
import           Control.Exception (throw)
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.Text as T
import           Utils ((<<), returnOrThrow)

newtype ParserState s = ParserState { stream :: s }
                      deriving (Show, Eq)

newtype ParserError = ParserError Text
                    deriving (Show, Eq, Typeable)

instance Exception ParserError

newtype ParserT s m a = ParserT
  { runParserT :: StateT (ParserState s) (ExceptT ParserError m) a }

instance Functor m => Functor (ParserT s m) where
  fmap f (ParserT s) = ParserT $ fmap f s

instance Monad m => Applicative (ParserT s m) where
  pure = ParserT . pure
  (ParserT f) <*> (ParserT a) = ParserT $ f <*> a

instance Monad m => Monad (ParserT s m) where
  (ParserT m) >>= f = ParserT $ m >>= runParserT . f

instance Monad m => MonadState (ParserState s) (ParserT s m) where
  get = ParserT get
  put = ParserT . put

instance Monad m => MonadError ParserError (ParserT s m) where
  throwError = ParserT . throwError
  ParserT p `catchError` f = ParserT $ p `catchError` (runParserT . f)

instance MonadTrans (ParserT s) where
  lift = ParserT . lift . lift

instance Monad m => Alternative (ParserT s m) where
  empty = fail "empty" -- not really an identity
  p1 <|> p2 = p1 `catchError` \(_ :: ParserError) -> p2

preview :: (Monad m, S.Stream s a) => ParserT s m (Maybe a)
preview = do
  ParserState stream <- get
  return $ fst <$> S.read stream

consume :: (Monad m, S.Stream s a) => ParserT s m ()
consume = do
  ParserState stream <- get
  case S.read stream of
    Nothing -> return ()
    Just (_, s) -> put $ ParserState s

next :: (Monad m, S.Stream s a) => ParserT s m a
next = preview << consume >>= returnOrThrow (ParserError "unexpected end of stream")

atom :: (Monad m, S.Stream s a, Show a, Eq a) => a -> ParserT s m a
atom a = do
  a' <- next
  if a == a'
  then return a
  else fail $ "fail to parse " ++ show a

string :: (Monad m, S.Stream s a, Show s, Eq a) => s -> ParserT s m s
string s = go s $> s
  where
  go :: (Monad m, S.Stream s a, Show s, Eq a) => s -> ParserT s m ()
  go s' = case S.read s' of
            Nothing -> return ()
            Just (a, s'') -> do
              a' <- next
              if a == a'
              then go s''
              else fail $ "fail to parse " ++ show s

choose :: Monad m => [ParserT s m a] -> ParserT s m a
choose [] = empty
choose [x] = x
choose (x:xs) = x <|> choose xs

predicate :: (Monad m, S.Stream s a, Show a) => (a -> Bool) -> ParserT s m a
predicate f = do
  a <- next
  if f a
  then return a
  else fail $ "unexpected " ++ show a

many :: Monad m => ParserT s m a -> ParserT s m [a]
many parser = do
  maybe <- (Just <$> parser) `catchError` \(_ :: ParserError) -> return Nothing
  case maybe of
    Just a -> (a:) <$> many parser
    Nothing -> return []

fail :: Monad m => [Char] -> ParserT s m a
fail = throwError . ParserError . T.pack

optional :: Monad m => ParserT s m a -> ParserT s m ()
optional p = p $> () <|> return ()


execParserT :: (Monad m, S.Stream s a) => ParserT s m x -> s -> m x
execParserT parser s = do
  result <- runExceptT $ runStateT (runParserT parser) (ParserState s)
  case result of
    Left e -> throw e
    Right (a, _) -> return a
