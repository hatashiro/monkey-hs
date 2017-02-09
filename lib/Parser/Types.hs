module Parser.Types where

import Protolude

import           Lexer.Token
import           Unsafe (unsafeFromJust)
import           Utils ((<<))

data ParserState = ParserState { left :: [Token], done :: [Token] }

initState :: [Token] -> ParserState
initState = flip ParserState []

newtype ParserT e m a = ParserT { runParserT :: StateT ParserState (ExceptT e m) a }

instance Functor m => Functor (ParserT e m) where
  fmap f (ParserT s) = ParserT $ fmap f s

instance Monad m => Applicative (ParserT e m) where
  pure = ParserT . pure
  (ParserT f) <*> (ParserT a) = ParserT $ f <*> a

instance Monad m => Monad (ParserT e m) where
  (ParserT m) >>= f = ParserT $ m >>= runParserT . f

instance (Monoid e, Monad m) => Alternative (ParserT e m) where
  empty = undefined
  (<|>) = undefined

type ParseError = [Text]
type Parser = ParserT ParseError Identity

preview :: Parser (Maybe Token)
preview = ParserT $ go . left <$> get
  where
  go :: [Token] -> Maybe Token
  go (x:_) = Just x
  go [] = Nothing

consume :: Parser ()
consume = ParserT get >>= go
  where
  go :: ParserState -> Parser ()
  go (ParserState [] _) = return ()
  go (ParserState (x:xs) done) = ParserT $
    put $ ParserState xs (x:done)

next :: Parser Token
next = unsafeFromJust <$> preview << consume

runParser :: Parser a -> [Token] -> a
runParser = ((fst . runIdentity) .) . (. initState) . runStateT . runParserT
