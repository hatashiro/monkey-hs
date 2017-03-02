module Evaluator where

import Protolude

import Data.List (last)
import Evaluator.Object
import Evaluator.Types
import Parser.AST

evalProgram :: Program -> Evaluator Object
evalProgram (Program stmts) = last <$> traverse evalStmt stmts

evalStmt :: Stmt -> Evaluator Object
evalStmt (ExprStmt expr) = evalExpr expr
evalStmt _ = undefined

evalExpr :: Expr -> Evaluator Object
evalExpr (LitExpr l) = evalLiteral l
evalExpr (PrefixExpr p e) = evalPrefix p e
evalExpr _ = undefined

evalLiteral :: Literal -> Evaluator Object
evalLiteral (IntLiteral i) = return $ OInt i
evalLiteral (BoolLiteral b) = return $ OBool b

evalPrefix :: Prefix -> Expr -> Evaluator Object
evalPrefix Not e = do
  o <- evalExpr e
  return $ OBool . not . o2b $ o
evalPrefix PrefixPlus e = undefined
evalPrefix PrefixMinus e = undefined

o2b :: Object -> Bool
o2b (OBool b) = b
o2b (OInt i) = i /= 0
o2b (ONull) = False

eval :: Program -> Either EvalError Object
eval = execEvaluator . evalProgram
