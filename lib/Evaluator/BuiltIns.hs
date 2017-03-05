module Evaluator.BuiltIns where

import Protolude

import qualified Data.Text as T
import           Evaluator.Object
import           Parser.AST (Ident(Ident))

invalid :: Object -> [Object] -> IO Object
invalid f args = return . OBuiltInError $
  "invalid arguments for " <> show f <> ": " <> show args

len :: Object
len = OBuiltInFn "len" 1 go
  where
  go :: [Object] -> IO Object
  go [OString t] = return . OInt . fromIntegral $ T.length t
  go args = invalid len args

builtIns :: [(Ident, Object)]
builtIns = [ (Ident "len", len)
           ]
