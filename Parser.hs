module Parser where

import Tokenizer
import Prelude hiding (lookup)

data AST = ASum Operator AST AST
         | AProd Operator AST AST
         | AAssign String AST
         | ANeg AST
         | AExp AST AST
         | ANum Integer
         | AIdent String

parse :: String -> Maybe AST
parse input =
  let ts = tokenize input in
  case ts of
    [TEof] -> Nothing
    _ -> let (tree, ts') = expression ts in
         if ts' == [TEof]
         then Just tree
         else error ("Parsing error on: " ++ show ts')

expression :: [Token] -> (AST, [Token])
expression ts =
  let (termNode, ts') = term ts in
  case lookup ts' of
    TOp op | op == Plus || op == Minus ->
      let (exprNode, ts'') = expression $ accept ts' in
      (ASum op termNode exprNode, ts'')
    TAssign ->
      case termNode of
        AIdent v -> let (exprNode, ts'') = expression $ accept ts' in
                    (AAssign v exprNode, ts'')
        _ -> error "Syntax error: assignment is only possible to identifiers"
    _ -> (termNode, ts')

term :: [Token] -> (AST, [Token])
term ts =
  let (factNode, ts') = expterm ts in
  case lookup ts' of
    TOp op | op == Mult || op == Div ->
      let (termNode, ts'') = term $ accept ts' in
      (AProd op factNode termNode, ts'')
    _ -> (factNode, ts')

expterm :: [Token] -> (AST, [Token])
expterm ts =
  let (factNode, ts') = factor ts in
  case lookup ts' of
    TOp op | op == Exp ->
      let (termNode, ts'') = expterm $ accept ts' in
      (AExp factNode termNode, ts'')
    _ -> (factNode, ts')

factor :: [Token] -> (AST, [Token])
factor ts =
  case lookup ts of
    TLParen ->
      let (exprNode, ts') = expression $ accept ts in
      case lookup ts' of
        TRParen -> (exprNode, accept ts')
        _ -> error "Syntax error: mismatched parentheses"
    TIdent v -> (AIdent v, accept ts)
    TNum d -> (ANum d, accept ts)
    TOp o | o == Minus -> let (a,b) = factor (accept ts) in
                                  (ANeg a, b)
    _ -> error "Syntax error: factor can only be a digit, an identifier or a parenthesised expression"

lookup :: [Token] -> Token
lookup = head

accept :: [Token] -> [Token]
accept = tail

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> show i
                  ANeg   i     -> "Unary " ++ showOp Minus : '\n' : show' (ident n) i
                  AExp     l r -> '^' : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r)
      ident = (+1)
      showOp Plus  = '+'
      showOp Minus = '-'
      showOp Mult  = '*'
      showOp Div   = '/'
      showOp Exp   = '^'
