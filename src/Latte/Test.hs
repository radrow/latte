module Latte.Test where

import qualified Data.Text.IO as T

-- import Latte.Parse as P
-- import Latte.PP
-- import Latte.Validator
-- import Latte.LLVM as L
-- import Latte.Types.AST
-- import Llvm
-- import Unique
-- import Outputable
-- import FastString
-- import Data.Map as M
-- import Control.Monad.State
-- import Control.Monad.Reader
-- import Control.Monad.Except

-- test p inp =
--   case runParser p "" inp of
--     Left e -> putStrLn e
--     Right x -> print x


-- testFile f = do
--   src <- T.readFile f
--   case runParser P.program f src of
--     Left e -> putStrLn e
--     Right p -> putStrLn $ ppProgram $ entailProgram p


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

-- makePrg es = do
--   parsed <- runParser P.program "test" es
--   typed <- tcProgram (entailProgram parsed)
--   L.program typed

-- testPrg es = do
--   case makePrg es of
--     Left e -> putStrLn e
--     Right ss -> putStrLn $ showSDocUnsafe $ ppLlvmModule $ ss
