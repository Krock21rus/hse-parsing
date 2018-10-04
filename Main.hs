module Main where

import Parser

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do
  runParser " 1 - 2 - 3 "
  runParser " (((9)))"
  runParser " 1 * 2 - 3 / 4 + 5"
  runParser "(var = 13 * 42)"
  runParser "(var = -13 * -42 + -142 - 143)"
  runParser "(var = -13 * -42 + -142 - 143 + 5^6^(-7))"
