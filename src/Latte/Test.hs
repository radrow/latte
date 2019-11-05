module Latte.Test where

import qualified Data.Text.IO as T

import Latte.Parse as P
import Latte.PP
import Latte.LLVM as L
import Latte.Types.Free
import Llvm
import Unique
import Outputable
import FastString
import Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

test p inp =
  case runParser p "" inp of
    Left e -> putStrLn e
    Right x -> print x


testFile f = do
  src <- T.readFile f
  case runParser program f src of
    Left e -> putStrLn e
    Right p -> putStrLn $ prettyPrint $ programM p


-- testLLStmts es = do
--   case runParser P.stmt "test" es
--     of Left e -> putStrLn e
--        Right p ->
--          case fst <$> runReader (runExceptT (evalState (L.stmt $ stmtM p) (SupplyState M.empty newSupply))) newEnv of
--            Right ss ->
--              putStrLn $ showSDocUnsafe $ ppLlvmModule $ LlvmModule
--              []
--              []
--              []
--              []
--              []
--              [ LlvmFunction
--                ( LlvmFunctionDecl
--                  (fsLit "XD")
--                  Internal
--                  CC_Ccc
--                  i32
--                  FixedArgs
--                  []
--                  Nothing
--                )
--                []
--                []
--                Nothing
--                Nothing
--                       [
--                         LlvmBlock
--                         (mkCoVarUnique 2137)
--                         ss
--                       ]
--              ]
--            Left s -> putStrLn s


testLLFun es = do
  case runParser P.topDef "test" es
    of Left e -> putStrLn e
       Right p ->
         case runExcept (evalState (L.topDef $ topDefM p) (SupplyState M.empty newSupply)) of
           Left e -> putStrLn e
           Right ss -> putStrLn $ showSDocUnsafe $ ppLlvmModule $ LlvmModule
             []
             []
             []
             []
             []
             ss
