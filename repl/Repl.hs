{-# LANGUAGE ScopedTypeVariables #-}
module Repl where

import Protolude hiding (evaluate)

import qualified Data.Text as T
import           Lexer (lex)
import           Parser (parse)
import           Evaluator (evalWithState)
import           Evaluator.Types (EvalError, EvalState, createEmptyState)
import           Evaluator.Object (Object)
import           Common.ParserT (ParserError)
import           System.Console.Haskeline
import qualified GHC.Show as G

loop :: (a -> IO a) -> a -> IO a
loop io a = io a >>= loop io

read :: IO (Maybe Text)
read = runInputT defaultSettings $ (fmap . fmap) toS $ getInputLine "> "

data InterpretError = P ParserError | E EvalError

instance G.Show InterpretError where
  show (P p) = show p
  show (E p) = show p

evaluate :: EvalState -> Text -> IO (Either InterpretError (Object, EvalState))
evaluate state input = do
  let result = lex input >>= parse
  case result of
    Right ast -> first E <$> evalWithState ast state
    Left err -> return $ Left (P err)

rep :: EvalState -> IO EvalState
rep state = do
  maybeText <- read
  case maybeText of
    Just text | not $ T.null text -> do
      result <- evaluate state text
      case result of
        Left err -> do
          putStrLn (show err :: Text)
          return state
        Right (object, state') -> do
          putStrLn $ (show object :: Text)
          return state'
    _ -> return state

repl :: IO ()
repl = (createEmptyState >>= loop rep) $> ()
