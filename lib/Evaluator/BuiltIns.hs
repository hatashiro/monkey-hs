module Evaluator.BuiltIns where

import Protolude

import qualified Data.Text as T
import           Evaluator.Object
import           Parser.AST (Ident(Ident))

breturn :: Object -> IO BuiltInFnResult
breturn = return . Right

bthrow :: Text -> IO BuiltInFnResult
bthrow = return . Left

invalid :: Object -> Text -> IO BuiltInFnResult
invalid f argText = bthrow $
  "invalid arguments for " <> show f <> ": " <> argText

len :: Object
len = OBuiltInFn "len" 1 go
  where
  go :: BuiltInFn
  go [OString t] = breturn . OInt . fromIntegral $ T.length t
  go [OArray arr] = breturn . OInt . fromIntegral $ length arr
  go args = invalid len $ show args

bhead :: Object
bhead = OBuiltInFn "head" 1 go
  where
  go :: BuiltInFn
  go [OArray []] = invalid bhead "empty array"
  go [OArray (x:_)] = breturn x
  go args = invalid bhead $ show args

btail :: Object
btail = OBuiltInFn "tail" 1 go
  where
  go :: BuiltInFn
  go [OArray []] = invalid btail "empty array"
  go [OArray (_:xs)] = breturn $ OArray xs
  go args = invalid btail $ show args

bcons :: Object
bcons = OBuiltInFn "cons" 2 go
  where
  go :: BuiltInFn
  go [o, OArray os] = breturn . OArray $ o:os
  go args = invalid bcons $ show args

bprint :: Object
bprint = OBuiltInFn "print" 1 go
  where
  go :: BuiltInFn
  go [OString t] = putStrLn t >> breturn nil
  go [o] = putStrLn (show o :: Text) >> breturn nil
  go args = invalid bprint $ show args

builtIns :: [(Ident, Object)]
builtIns = [ (Ident "len", len)
           , (Ident "head", bhead)
           , (Ident "tail", btail)
           , (Ident "cons", bcons)
           , (Ident "print", bprint)
           ]
