module Tokenizer where

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Exp
              deriving (Show, Eq)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Exp
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

isNumber :: String -> Bool
isNumber x = all isDigit x

number :: String -> Integer
number c = read c :: Integer

isElem :: Char -> Bool
isElem c = (isDigit c) || (isAlpha c)

isIdent :: String -> Bool
isIdent [] = False
isIdent (c : cs) = (isAlpha c) && (all isElem cs)

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"
