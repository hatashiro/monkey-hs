module Main where

import Protolude hiding (evaluate)

import           Common.ParserT (ParserError)
import           Evaluator (eval)
import           Lexer (lex)
import           Parser (parse)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= traverse_ evalFile

evalFile :: FilePath -> IO ()
evalFile file = readFile file >>= evaluate

printError :: Exception e => e -> IO ()
printError e = putStrLn (show e :: Text)

evaluate :: Text -> IO ()
evaluate input = do
  let parsed = lex input >>= parse
  case parsed of
    Left err -> printError err
    Right ast -> do
      evaluated <- eval ast
      case evaluated of
        Left err -> printError err
        _ -> return ()
