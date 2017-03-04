module Evaluator.Object where

import Protolude hiding (Show, show)

import qualified Data.Map.Strict as M
import           GHC.Show (Show(..))
import           Parser.AST

data Object = OInt Integer
            | OBool Bool
            | ONull
            | OFn { params :: [Ident]
                  , body :: BlockStmt
                  , env :: Environment
                  }
            | OReturn Object
            deriving (Eq)

instance Show Object where
  show (OInt x) = show x
  show (OBool x) = if x then "true" else "false"
  show ONull        = "null"

true :: Object
true = OBool True

false :: Object
false = OBool False

nil :: Object
nil = ONull

ret :: Object -> Object
ret o = OReturn o

isReturned :: Object -> Bool
isReturned (OReturn _) = True
isReturned _           = False

returned :: Object -> Object
returned (OReturn o) = o
returned o           = o

type Environment = M.Map Ident Object
