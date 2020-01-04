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
eStruct = "class dup { int f; int g;} dup f(dup d) { dup p; p.f = d.f + d.g; p.g = 10; return p; } int main() { dup d; d.f = 21; d.g = 34; printInt(f(d).f); return 0; }"

eStruct2 :: String
eStruct2 = unlines
  [ "class point { int x, y; }"
  , "class line { point p1, p2; }"
  , "int len(line l) { int dx = l.p1.x - l.p2.x, dy = l.p1.y - l.p2.y; return dx*dx+dy*dy; }"
  , "int main() { point p1, p2; p1.x = 1; p1.y = 1; p2.x = 2; p2.y = 11; line l;"
  , "l.p1 = p1; l.p2 = p2; printInt(len(l)); return 1; }"
  ]
