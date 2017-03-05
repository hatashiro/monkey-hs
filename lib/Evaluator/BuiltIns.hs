module Evaluator.BuiltIns where

import Protolude

import qualified Data.Text as T
import           Evaluator.Object
import           Parser.AST (Ident(Ident))

breturn :: Object -> IO BuiltInFnResult
breturn = return . Right

bthrow :: Text -> IO BuiltInFnResult
bthrow = return . Left

invalid :: Object -> [Object] -> IO BuiltInFnResult
invalid f args = bthrow $
  "invalid arguments for " <> show f <> ": " <> show args

len :: Object
len = OBuiltInFn "len" 1 go
  where
  go :: BuiltInFn
  go [OString t] = breturn . OInt . fromIntegral $ T.length t
  go args = invalid len args

bprint :: Object
bprint = OBuiltInFn "print" 1 go
  where
  go :: BuiltInFn
  go [OString t] = putStrLn t >> breturn nil
  go [o] = putStrLn (show o :: Text) >> breturn nil
  go args = invalid bprint args

builtIns :: [(Ident, Object)]
builtIns = [ (Ident "len", len)
           , (Ident "print", bprint)
           ]
