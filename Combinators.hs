module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem)
import Tokenizer

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
              deriving (Show)

-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

chopspace :: Parser Char
chopspace =
  char ' ' -<|> char '\t' -<|> char '\n'

parsespace :: Parser Char
parsespace =
  chopspace ->>= \a ->
  (
    parsespace -<|> return ' '
  )

untilendl :: Parser Char
untilendl =
  notchar '\n' -|>
  (
    untilendl -<|> return ' '
  )

untilend :: Parser Char
untilend =
  (chopchar -|> untilend) -<|> return ' '

untilcomm2 :: Parser Char
untilcomm2 =
  (char '*' -|> char '/') -<|> (chopchar -|> untilcomm2)

parsecomm1 :: Parser Char
parsecomm1 =
  char '/' -|> char '/' -|> untilendl

parsecomm2 :: Parser Char
parsecomm2 =
  char '/' -|> char '*' -|> untilcomm2

parsecomm3 :: Parser Char
parsecomm3 =
  char '/' -|> char '!' -|> untilend

parseempty :: Parser Char
parseempty =
  ((parsespace -<|> parsecomm1 -<|> parsecomm2 -<|> parsecomm3) -|> parseempty)
  -<|> return ' '

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 -<|>
(-<|>) :: Parser a -> Parser a -> Parser a
p -<|> q = \inp ->
  case p inp of
    Error _ -> q inp
    result  -> result

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 ->>=
(->>=) :: Parser a -> (a -> Parser b ) -> Parser b
p ->>= q = \inp ->
  let inp' = inp in
    case p inp' of
      Success (r, inp'') -> q r inp''
      Error err -> Error err

-- Sequential combinator which ignores the result of the first parser
infixl 7 -|>
(-|>) :: Parser a -> Parser b -> Parser b
p -|> q = p ->>= const q

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = (parseempty -|> p) -<|> (parseempty -|> q)

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  let Success (c, inp') = parseempty inp in
    case p inp' of
      Success (r, inp'') ->
        let Success (c, inp''') = parseempty inp'' in
          q r inp'''
      Error err -> Error err

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q


-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops of the first elem of the string
chopchar :: Parser Char
chopchar (c : cs) = Success (c, cs)
chopchar [] = Error "Empty string"

-- Chops of the first elem of the string
elem :: Parser String
elem [] = Error "Empty string"
elem cs = let (a, b) = (span isElem cs) in
              case a of
                [] -> Error "Empty string"
                a -> Success (a, b)

-- Checks if the first character of the string is the given one
char :: Char -> Parser Char
char c = sat (== c) chopchar

-- Checks if the first character of the string is the given one
notchar :: Char -> Parser Char
notchar c = sat (/= c) chopchar


-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error "Predicate is not satisfied"
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser inp of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err
