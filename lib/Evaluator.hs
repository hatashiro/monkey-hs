module Evaluator where

import Protolude

import           Evaluator.Object
import           Evaluator.Types
import qualified Parser.AST as A

eval :: A.Program -> Either EvalError Object
eval = undefined
