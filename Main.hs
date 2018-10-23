{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser "a = b + c; // abc\n d = e"
  runParser "a = b + c /* 12c3r4wx5v34iuh ! * *  */ "
  runParser "a = /* 2vtc43243 ! * * */ b + c"
  runParser "a = b // fvw 234 f / * !\n + c; d = 2 /* fc2243 ! * * / */ - 3; e = 5/!"
  runParser "1/! + 2;\n// x = 13;\n777"
  runParser "1 + 2;\n/ / x = 13;\n777"
  runParser "a = /* 2vtc43243 ! * * */ b"
  runParser "abc /* 234vt 5 65**!/*1 24 t4*/ = /****/ def^2 /***/ + /***/ e /***/ ; /***/ // 4v2uy5c87wn452t//*88/8*/345 /!135v234/\ng=h/!nguwy4ic7y2n34ytwncmrjxi"
