module Repl where

import Protolude hiding (evaluate)

import Data.Text (pack)
import Lexer (lex)
import Lexer.Token (Token)
import System.Console.Haskeline

loop :: MonadIO m => m a -> m a
loop io = io >> loop io

read :: (MonadException m, MonadIO m) => m (Maybe Text)
read = runInputT defaultSettings $ (fmap . fmap) pack $ getInputLine "> "

evaluate :: MonadIO m => Text -> m Text
evaluate = pure . show . lex -- FIXME

rep :: (MonadException m, MonadIO m) => m ()
rep = do
  maybeText <- read
  case maybeText of
    Just "" -> return ()
    Nothing -> return ()
    Just text -> do
      result <- evaluate text
      putStrLn result

repl :: IO ()
repl = loop rep
