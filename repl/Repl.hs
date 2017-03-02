{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import Protolude hiding (evaluate)

import           Data.Text (pack)
import           Lexer (lex)
import           Parser (parse)
import           Evaluator (eval)
import           Evaluator.Types (EvalError)
import           Evaluator.Object (Object)
import           Common.ParserT (ParserError)
import           System.Console.Haskeline
import qualified GHC.Show as G

loop :: IO a -> IO a
loop io = io >> loop io

read :: IO (Maybe Text)
read = runInputT defaultSettings $ (fmap . fmap) pack $ getInputLine "> "

data InterpretError = P ParserError | E EvalError

instance G.Show InterpretError where
  show (P p) = show p
  show (E p) = show p

evaluate :: Text -> Either InterpretError Object
evaluate input = do
  ast <- first P $ lex input >>= parse
  first E $ eval ast

rep :: IO ()
rep = do
  maybeText <- read
  case maybeText of
    Just "" -> return ()
    Nothing -> return ()
    Just text -> do
      let result = evaluate text
      putStrLn $ case result of
        Left err -> (show err :: Text)
        Right object -> show object

repl :: IO ()
repl = loop rep
