module Latte.Test where

import Latte.Frontend.AST
import Latte.Frontend.Parse
import Latte.Frontend.Typechecker
import Latte.Frontend.IR
import qualified Latte.Backend.X86.Compile as X86


import Data.Text(pack)

testX86 :: String -> IO ()
testX86 s =
  case runLatteParser program "test" (pack s) >>= typecheck of
    Left e -> putStrLn e
    Right (ce, p) -> putStrLn $ pp (X86.compile $ compile ce p)

testIR :: String -> IO ()
testIR s =
  case runLatteParser program "test" (pack s) >>= typecheck of
    Left e -> putStrLn e
    Right (ce, p) -> putStrLn $ pp (compile ce p)

testAST :: String -> IO ()
testAST s =
  case runLatteParser program "test" (pack s) >>= typecheck of
    Left e -> putStrLn e
    Right (_, p) -> putStrLn $ pp p

e0 :: String
e0 = "int f(int x) { if (true) {return 2 + 2;} return 1;}"

e1 :: String
e1 = "int f(int x, bool y) { int k = 2, l = x + 1; if (y || k == x) {l++;} return l;}"

e2 :: String
e2 = "int f(int x) { int i = 0; if (x < 0) {return 0;} else {while(x != 0) {i = i + x; x--;}} return i;}"

eFactR :: String
eFactR = "int f(int x) { if (x < 2) return 1; return x * f(x - 1); }"

eFactI :: String
eFactI = "int f(int x) { int n = 1; while(x > 1) { n = n*x; x--;} return n; }"

eStruct :: String
eStruct = "class dup { public int f; } int f(dup d) { return d.f; }"
