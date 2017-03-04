module Evaluator.Object where

import Protolude hiding (Show, show)

import           Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import qualified Data.Map.Strict as M
import           GHC.Show (Show(..))
import           Parser.AST

data Object = OInt Integer
            | OBool Bool
            | ONull
            | OFn { params :: [Ident]
                  , body :: BlockStmt
                  , env :: EnvRef
                  }
            | OReturn Object
            deriving (Eq)

instance Show Object where
  show (OInt x) = show x
  show (OBool x) = if x then "true" else "false"
  show ONull = "null"
  show (OFn _ _ _) = "[function]"
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

type EnvRef = IORef Environment

data Environment = Environment { varMap :: M.Map Ident Object
                               , parent :: Maybe EnvRef
                               }
                 deriving (Eq)

emptyEnv :: IO EnvRef
emptyEnv = newIORef $ Environment M.empty Nothing

wrapEnv :: EnvRef -> [(Ident, Object)] -> IO EnvRef
wrapEnv = (newIORef .) . flip (Environment . M.fromList) . Just

insertVar :: Ident -> Object -> EnvRef -> IO EnvRef
insertVar i o ref = modifyIORef ref (go i o) $> ref
  where
  go :: Ident -> Object -> Environment -> Environment
  go i o (Environment m p) = Environment (M.insert i o m) p

getVar :: Ident -> EnvRef -> IO (Maybe Object)
getVar i ref = do
  Environment m p <- readIORef ref
  case M.lookup i m of
    Just o -> return $ Just o
    Nothing -> case p of
                 Just parent -> getVar i parent
                 Nothing -> return $ Nothing
