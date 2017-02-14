module Parser where

import Protolude hiding (one, many)

import           Common.ParserT
import qualified Lexer.Token as Tk
import           Parser.AST
import           Parser.Types
import           Utils ((<<))

parseProgram :: Parser Program
parseProgram = Program <$> many parseStmt

parseStmt :: Parser Stmt
parseStmt = choose
  [ parseLetStmt
  ]

parseIdent :: Parser Ident
parseIdent = next >>= \tkn ->
  case tkn of
    Tk.Ident name -> return $ Ident name
    _ -> fail "fail to parse an identifier"

parseLetStmt :: Parser Stmt
parseLetStmt = do
  atom Tk.Let
  ident <- parseIdent
  atom Tk.Assign
  expr <- parseExpr
  atom Tk.SemiColon
  return $ LetStmt ident expr

parseExpr :: Parser Expr
parseExpr = choose
  [ parseLitExpr
  ]

parseLitExpr :: Parser Expr
parseLitExpr = fmap LitExpr $ next >>= \tkn ->
  case tkn of
    Tk.IntLiteral i -> return $ IntLiteral i
    Tk.BoolLiteral b -> return $ BoolLiteral b
    _ -> fail "fail to parse a literal"

parse :: [Tk.Token] -> Program
parse = execParser (parseProgram << atom Tk.EOF)
