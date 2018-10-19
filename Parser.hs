module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String
         | ANeg AST
         | AExp AST AST
         | AProgram AST AST
         | AList AST AST
         | AConcat AST AST
         | AEmpty

-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _ -> case program input of
           Success (tree, ts') ->
             if null (getclean ts')
             then Just (Success tree)
             else Just (Error ("Syntax error on: " ++ show ts')) -- Only a prefix of the input is parsed
           Error err -> Just (Error err) -- Legitimate syntax error

isnotendl :: Char -> Bool
isnotendl c = not (c == '\n')

droponechar :: String -> String
droponechar (c:cs) = cs
droponechar cs = cs

mydrop2 :: String -> String
mydrop2 (c:cs) =
  case c of
    '/' -> cs
    _ -> mydrop cs
mydrop2 cs = cs


mydrop :: String -> String
mydrop (c:cs) =
  case c of
    '*' -> mydrop2 cs
    _ -> mydrop cs
mydrop cs = cs

cleancomm :: Parser AST
cleancomm inp =
  let inp' = (dropWhile isWhiteSpace inp) in
    case char '/' inp' of
      Success (a, inp'') ->
        case chopchar inp'' of
          Success ('/', _) -> Success ('', cleancomm (dropWhile isnotendl (droponechar inp'')))
          Success ('*', _) -> Success ('', cleancomm (mydrop inp''))
          Success ('!', _) -> Success ('', "")
          Success (a, b) -> Success ('', inp')
          Error _ -> Success ('', inp')
      Error _ -> Success ('', inp')

program :: Parser AST
program =
  cleancomm |> expression >>= \l ->
  (
    (
    cleancomm |> progOp     >>= \op ->
    cleancomm |> program    >>= \r -> return (AProgram l r)
    )
    <|>
    return l
  )

expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    cleancomm |> assignment |>
    cleancomm |> expression >>= \e -> return (AAssign i e)
  )
  -- <|>
  -- ( identifier >>= \(AIdent i) ->
  --   assignment |>
  --   listterm >>= \e -> return (AAssign i e)
  -- )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        cleancomm |> plusMinus  >>= \op ->
        cleancomm |> expression >>= \r  -> return (ASum op l r)
      )
  -- <|> listterm
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  expterm >>= \l ->
  ( ( cleancomm |> divMult >>= \op ->
      cleancomm |> term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

expterm :: Parser AST
expterm =
  factor >>= \l ->
  ( ( cleancomm |> expOp |>
      (cleancomm |> expterm    >>= \r  -> return (AExp l r))
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    cleancomm |> expression >>= \e ->
    cleancomm |> rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number
  <|> (char '-' |> factor >>= \e -> return (ANeg e))

inlist :: Parser AST
inlist =
  (
  expression >>= \e1 ->
  comma |>
  inlist >>= \e2 -> return (AList e1 e2)
  )
  <|> (expression >>= \e -> return (AList e AEmpty))
  <|> return (AList AEmpty AEmpty)

onelist :: Parser AST
onelist =
  ( lbracket |>
    inlist >>= \e ->
    rbracket |> return e
  )
  <|> identifier

listterm :: Parser AST
listterm =
  (
  onelist >>= \a ->
  char '+' -|> char '+' -- -|> is |> without drop spaces
  |> listterm >>= \b -> return (AConcat a b)
  )
  <|> onelist

number :: Parser AST
number      = map (ANum   . T.number) (sat T.isNumber elem)

identifier :: Parser AST
identifier = map (AIdent) (sat T.isIdent elem)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

lbracket :: Parser Char
lbracket = char '['

rbracket :: Parser Char
rbracket = char ']'

comma :: Parser Char
comma = char ','

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

expOp :: Parser T.Operator
expOp   = map T.operator (char '^')

progOp :: Parser T.Operator
progOp  = map T.operator (char ';')




instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AExp     l r -> showOp T.Exp : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> show i
                  ANeg   e     -> "Unary -\n" ++ show' (ident n) e
                  AProgram l r -> showOp T.Semicolon : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AList    l r -> "List\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AConcat  l r -> "Concat\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AEmpty       -> "";)
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Exp   = '^'
      showOp T.Semicolon = ';'
