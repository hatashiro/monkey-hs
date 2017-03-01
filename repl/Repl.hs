{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import Protolude hiding (evaluate)

import Data.Text (pack)
import Lexer (lex)
import Parser (parse)
import Common.ParserT (ParserError)
import System.Console.Haskeline hiding (catch)

loop :: MonadIO m => m a -> m a
loop io = io >> loop io

read :: (MonadException m, MonadIO m) => m (Maybe Text)
read = runInputT defaultSettings $ (fmap . fmap) pack $ getInputLine "> "

evaluate :: MonadIO m => Text -> m Text
evaluate input = do
  let tokens = lex input
  let ast = parse tokens
  return $ show ast

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
