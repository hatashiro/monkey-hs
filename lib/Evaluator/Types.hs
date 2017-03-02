module Evaluator.Types where

import Protolude

newtype EvalError = EvalError Text
                    deriving (Show, Eq, Typeable)

instance Exception EvalError
