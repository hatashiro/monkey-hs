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
evalExpr (InfixExpr i l r) = evalInfix i l r
evalExpr _ = undefined

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

o2b :: Object -> Evaluator Bool
o2b (OBool b) = return b
o2b o = throwError . EvalError $ show o <> " is not a bool"

o2n :: Object -> Evaluator Integer
o2n (OInt i) = return i
o2n o = throwError . EvalError $ show o <> " is not a number"

ee2x :: (a -> a -> b) -> (Object -> Evaluator a) -> Expr -> Expr -> Evaluator b
ee2x f = (liftM2 f `on`) . (evalExpr >=>)

eval :: Program -> Either EvalError Object
eval = execEvaluator . evalProgram
