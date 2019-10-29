module Latte.Test where

import qualified Data.Text.IO as T

import Latte.Parse
import Latte.PP
import Latte.Types.AST


test p inp =
  case runParser p "" inp of
    Left e -> putStrLn e
    Right x -> print x


testFile f = do
  src <- T.readFile f
  case runParser program "test" src of
    Left e -> putStrLn e
    Right p -> putStrLn $ ppProgram $ entailProgram p
