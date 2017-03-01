{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import Protolude hiding (evaluate, catch)

import Control.Exception (catch)
import Data.Text (pack)
import Lexer (lex)
import Parser (parse)
import Common.ParserT (ParserError)
import System.Console.Haskeline hiding (catch)

loop :: IO a -> IO a
loop io = io >> loop io

read :: IO (Maybe Text)
read = runInputT defaultSettings $ (fmap . fmap) pack $ getInputLine "> "

evaluate :: Text -> Either ParserError Text
evaluate input = do
  tokens <- lex input
  ast <- parse tokens
  return $ show ast

rep :: IO ()
rep = do
  maybeText <- read
  case maybeText of
    Just "" -> return ()
    Nothing -> return ()
    Just text -> do
      let result = evaluate text
      putStrLn $ case result of
        Left err -> show err
        Right result -> result

repl :: IO ()
repl = loop rep
