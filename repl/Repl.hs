module Repl where

import Protolude hiding (putStr, evaluate)

import Data.Text.IO (putStr, getLine)
import Lexer (lex)
import Lexer.Token (Token)
import System.IO (hFlush, stdout)

loop :: MonadIO m => m a -> m a
loop io = io >> loop io

prompt :: Text
prompt = "> "

read :: MonadIO m => m Text
read = liftIO $ putStr prompt >> hFlush stdout >> getLine

evaluate :: MonadIO m => Text -> m Text
evaluate = pure . show . lex -- FIXME

rep :: MonadIO m => m ()
rep = do
  text <- read
  result <- evaluate text
  putStrLn result

repl :: IO ()
repl = loop rep
