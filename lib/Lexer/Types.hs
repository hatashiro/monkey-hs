module Lexer.Types where

import Protolude

import qualified Data.Text as T
import           Unsafe (unsafeFromJust)
import           Utils ((<<))

data LexerState = LexerState { left :: Text, done :: Text }

initState :: Text -> LexerState
initState = flip LexerState ""

newtype LexerT m a = LexerT { runLexerT :: StateT LexerState m a }

instance Functor m => Functor (LexerT m) where
  fmap f (LexerT s) = LexerT $ fmap f s

instance Monad m => Applicative (LexerT m) where
  pure = LexerT . pure
  (LexerT f) <*> (LexerT a) = LexerT $ f <*> a

instance Monad m => Monad (LexerT m) where
  (LexerT m) >>= f = LexerT $ m >>= runLexerT . f

type Lexer = LexerT Identity

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

next :: Lexer Char
next = unsafeFromJust <$> preview << consume

runLexer :: Lexer a -> Text -> a
runLexer = ((fst . runIdentity) .) . (. initState) . runStateT . runLexerT
