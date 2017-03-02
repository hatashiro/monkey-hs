module Evaluator.Object where

import Protolude hiding (Show, show)
import GHC.Show (Show(..))

data Object = OInt Integer
            | OBool Bool
            | ONull
            deriving (Eq)

instance Show Object where
  show (OInt x) = show x
  show (OBool x) = if x then "true" else "false"
  show ONull        = "null"

true :: Object
true = OBool True

false :: Object
false = OBool False
