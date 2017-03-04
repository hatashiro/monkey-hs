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
  show ONull = "null"
  show (OFn _ _ _) = "a function expression"
  show (OReturn o) = show o

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

data Environment = Environment { varMap :: M.Map Ident Object
                               , parent :: Maybe Environment
                               }
                 deriving (Eq)

emptyEnv :: Environment
emptyEnv = Environment M.empty Nothing

wrap :: Environment -> Environment
wrap = Environment M.empty . Just

insertVar :: Ident -> Object -> Environment -> Environment
insertVar i o (Environment m p) = Environment (M.insert i o m) p

getVar :: Ident -> Environment -> Maybe Object
getVar i (Environment m p) =
  case M.lookup i m of
    Just o -> Just o
    Nothing -> p >>= getVar i
