module Evaluator.Object where

import Protolude hiding (Show, show)

import           Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import qualified Data.Map.Strict as M
import           GHC.Show (Show(..))
import           Parser.AST

data Object = OInt Integer
            | OBool Bool
            | OString Text
            | OArray [Object]
            | OHash (M.Map Hashable Object)
            | ONull
            | OFn { params :: [Ident]
                  , body :: BlockStmt
                  , env :: EnvRef
                  }
            | OBuiltInFn { name :: Text
                         , numParams :: Int
                         , fn :: BuiltInFn
                         }
            | OReturn Object

instance Show Object where
  show (OInt x) = show x
  show (OBool x) = if x then "true" else "false"
  show (OString x) = show x
  show (OArray x) = show x
  show (OHash m) = "{" ++ go (M.toList m) ++ "}"
    where
    go [(l, o)] = show l ++ ":" ++ show o
    go (x:xs) = go [x] ++ "," ++ go xs
  show ONull = "null"
  show (OFn _ _ _) = "[function]"
  show (OBuiltInFn n _ _) = "[built-in function: " ++ toS n ++ "]"
  show (OReturn o) = show o

instance Eq Object where
  OInt x == OInt y = x == y
  OBool x == OBool y = x == y
  OString x == OString y = x == y
  OArray x == OArray y = x == y
  OHash x == OHash y = x == y
  ONull == ONull = True
  OFn p b e == OFn p' b' e' = p == p' && b == b' && e == e'
  OReturn o == o' = o == o'
  o == OReturn o' = o == o'
  OBuiltInFn n p _ == OBuiltInFn n' p' _ = n == n' && p == p'
  _ == _ = False

data Hashable = IntHash Integer
              | BoolHash Bool
              | StringHash Text
              deriving (Eq, Ord)

instance Show Hashable where
  show (IntHash i) = show $ OInt i
  show (BoolHash b) = show $ OBool b
  show (StringHash t) = show $ OString t

type BuiltInFnResult = Either Text Object
type BuiltInFn = [Object] -> IO BuiltInFnResult

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
