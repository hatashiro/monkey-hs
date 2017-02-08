module Main where

import Protolude
import Repl (repl)

main :: IO ()
main = putStrLn ("The Monkey programming language REPL" :: Text) >> repl
