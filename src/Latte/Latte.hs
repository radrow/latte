module Latte.Latte where

import Latte.Frontend.Parse

import Latte.Frontend.AST
import Latte.Frontend.Typechecker
import qualified Latte.Frontend.IR as IR
import qualified Latte.Backend.X86.Compile as X86
import Latte.Pretty

import Data.Text as T

buildX86 :: FilePath -> Text -> Either String String
buildX86 fname src =
  case runLatteParser program fname src >>= typecheck of
    Left e -> Left e
    Right (ce, p) -> Right $ pp (uncurry (flip X86.compile) $ IR.compile ce p)


buildIR :: FilePath -> Text -> Either String String
buildIR fname src =
  case runLatteParser program fname src >>= typecheck of
    Left e -> Left e
    Right (ce, p) -> Right $ pp $ fst $ IR.compile ce p
