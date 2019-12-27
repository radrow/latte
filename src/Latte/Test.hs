module Latte.Test where

import Latte.Frontend.AST
import Latte.Frontend.Parse
import Latte.Frontend.Typechecker
import Latte.Frontend.IR
import qualified Latte.Backend.X86.Compile as X86


import Data.Text(pack)

testX86 :: String -> IO ()
testX86 s =
  case runLatteParser program "test" (pack s) >>= tcProgram of
    Left e -> putStrLn e
    Right p -> putStrLn $ pp (X86.compile $ compile p)

testIR :: String -> IO ()
testIR s =
  case runLatteParser program "test" (pack s) >>= tcProgram of
    Left e -> putStrLn e
    Right p -> putStrLn $ pp (compile p)

testAST :: String -> IO ()
testAST s =
  case runLatteParser program "test" (pack s) >>= tcProgram of
    Left e -> putStrLn e
    Right p -> putStrLn $ pp p

e0 :: String
e0 = "int f(int x) { if (true) {return 2 + 2;} return 1;}"

e1 :: String
e1 = "int f(int x, bool y) { int k = 2, l = x + 1; if (y || k == x) {l++;} return l;}"

e2 :: String
e2 = "int f(int x) { int i = 0; if (x < 0) {return 0;} else {while(x != 0) {i = i + x; x--;}} return i;}"
