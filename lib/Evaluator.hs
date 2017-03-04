module Evaluator where

import Protolude

import           Data.List (last)
import qualified Data.Map.Strict as M
import           Evaluator.Object
import           Evaluator.Types
import           Parser.AST

evalProgram :: Program -> Evaluator Object
evalProgram (Program blockStmt) = returned <$> evalBlockStmt blockStmt

evalBlockStmt :: BlockStmt -> Evaluator Object
evalBlockStmt [] = return nil
evalBlockStmt (s:[]) = evalStmt s
evalBlockStmt (s:ss) = do
  o <- evalStmt s
  if isReturned o
  then return o
  else evalBlockStmt ss

evalStmt :: Stmt -> Evaluator Object
evalStmt (ExprStmt expr) = evalExpr expr
evalStmt (ReturnStmt expr) = ret <$> evalExpr expr
evalStmt (LetStmt ident expr) = evalExpr expr >>= registerIdent ident

registerIdent :: Ident -> Object -> Evaluator Object
registerIdent ident o = do
  updateEnv $ M.insert ident o
  return o

evalError :: Text -> Evaluator a
evalError = throwError . EvalError

evalExpr :: Expr -> Evaluator Object
evalExpr (IdentExpr i) = evalIdent i
evalExpr (LitExpr l) = evalLiteral l
evalExpr (PrefixExpr p e) = evalPrefix p e
evalExpr (InfixExpr i l r) = evalInfix i l r
evalExpr (IfExpr cond conse maybeAlter) = evalIf cond conse maybeAlter
evalExpr _ = undefined

evalIdent :: Ident -> Evaluator Object
evalIdent i = do
  env <- getEnv
  case M.lookup i env of
    Just o -> return o
    Nothing -> evalError $ "identifier not found: " <> show i

evalLiteral :: Literal -> Evaluator Object
evalLiteral (IntLiteral i) = return $ OInt i
evalLiteral (BoolLiteral b) = return $ OBool b

evalPrefix :: Prefix -> Expr -> Evaluator Object
evalPrefix Not = fmap (OBool . not) . (evalExpr >=> o2b)
evalPrefix PrefixPlus = fmap OInt . (evalExpr >=> o2n)
evalPrefix PrefixMinus = fmap (OInt . negate) . (evalExpr >=> o2n)

evalInfix :: Infix -> Expr -> Expr -> Evaluator Object
evalInfix Plus = (fmap OInt .) . ee2x (+) o2n
evalInfix Minus = (fmap OInt .) . ee2x (-) o2n
evalInfix Multiply = (fmap OInt .) . ee2x (*) o2n
evalInfix Divide = (fmap OInt .) . ee2x div o2n
evalInfix Eq = (fmap OBool .) . ee2x (==) return
evalInfix NotEq = (fmap OBool  .) . ee2x (/=) return
evalInfix GreaterThan = (fmap OBool .) . ee2x (>) o2n
evalInfix LessThan = (fmap OBool .) . ee2x (<) o2n

evalIf :: Expr -> BlockStmt -> Maybe BlockStmt -> Evaluator Object
evalIf cond conse maybeAlter = do
  condBool <- evalExpr cond >>= o2b
  if condBool
  then evalBlockStmt conse
  else case maybeAlter of
         Just alter -> evalBlockStmt alter
         Nothing -> return nil

o2b :: Object -> Evaluator Bool
o2b (OBool b) = return b
o2b o = evalError $ show o <> " is not a bool"

o2n :: Object -> Evaluator Integer
o2n (OInt i) = return i
o2n o = evalError $ show o <> " is not a number"

ee2x :: (a -> a -> b) -> (Object -> Evaluator a) -> Expr -> Expr -> Evaluator b
ee2x f = (liftM2 f `on`) . (evalExpr >=>)

eval :: Program -> Either EvalError Object
eval = fmap fst <$> flip execEvaluator emptyState . evalProgram

evalWithState :: Program -> EvalState -> Either EvalError (Object, EvalState)
evalWithState = execEvaluator . evalProgram
