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
evalExpr _ = undefined

evalLiteral :: Literal -> Evaluator Object
evalLiteral (IntLiteral i) = return $ OInt i
evalLiteral (BoolLiteral b) = return $ OBool b

eval :: Program -> Either EvalError Object
eval = execEvaluator . evalProgram
