module Evaluator.Object where

import Protolude hiding (Show, show)
import GHC.Show (Show(..))

data Object = OInt Integer
            | OBool Bool
            | ONull
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
