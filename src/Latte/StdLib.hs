{-# LANGUAGE OverloadedStrings #-}

module Latte.StdLib(stdglobals, stddecls, stddefs, stdfenv, strlenDecl, strcatDecl, callocDecl) where

import Latte.Types.Latte

import Llvm
import FastString
import Unique

import qualified Data.Map as M


unique :: FastString -> Unique
unique = getUnique


printfDecl = LlvmFunctionDecl
  "printf"
  External
  CC_Ccc
  i32
  VarArgs
  [(LMPointer i8, [])]
  Nothing

scanfDecl = LlvmFunctionDecl
  "scanf"
  External
  CC_Ccc
  i32
  VarArgs
  [(LMPointer i8, [])]
  Nothing

exitDecl = LlvmFunctionDecl
  "exit"
  External
  CC_Ccc
  LMVoid
  FixedArgs
  [(i32, [])]
  Nothing

strlenDecl = LlvmFunctionDecl
  "strlen"
  External
  CC_Ccc
  i32
  FixedArgs
  [(LMPointer i8, [])]
  Nothing


strcatDecl = LlvmFunctionDecl
  "strcat"
  External
  CC_Ccc
  (LMPointer i8)
  FixedArgs
  [(LMPointer i8, []), (LMPointer i8, [])]
  Nothing


callocDecl = LlvmFunctionDecl
  "calloc"
  External
  CC_Ccc
  (LMPointer i8)
  FixedArgs
  [(i32, []), (i32, [])]
  Nothing

runtimeErrorStr = LMGlobal
  { getGlobalVar = LMGlobalVar "error_msg"
                   (LMPointer (LMArray 15 i8))
                   Internal
                   Nothing
                   (Just 1)
                   Constant
  , getGlobalValue = Just $ LMStaticStr
                     "runtime error\\0A"
                     (LMArray 15 i8)
  }

printIntStr = LMGlobal
  { getGlobalVar = LMGlobalVar "print_int_str"
                   (LMPointer (LMArray 4 i8))
                   Internal
                   Nothing
                   (Just 1)
                   Constant
  , getGlobalValue = Just $ LMStaticStr
                     "%d\\0A"
                     (LMArray 4 i8)
  }

readIntStr = LMGlobal
  { getGlobalVar = LMGlobalVar "read_int_str"
                   (LMPointer (LMArray 3 i8))
                   Internal
                   Nothing
                   (Just 1)
                   Constant
  , getGlobalValue = Just $ LMStaticStr
                     "%d"
                     (LMArray 3 i8)
  }

runtimeErrorDecl = LlvmFunctionDecl
  "error"
  Internal
  CC_Ccc
  LMVoid
  FixedArgs
  []
  Nothing

runtimeError = LlvmFunction
  runtimeErrorDecl
  []
  []
  Nothing
  Nothing
  [LlvmBlock (unique "runtimeError_enter")
    [ Assignment (LMNLocalVar "runtimeErrorPrintfStr" (LMPointer i8))
      (GetElemPtr True (getGlobalVar runtimeErrorStr)
       [ LMLitVar $ LMIntLit 0 i64
       , LMLitVar $ LMIntLit 0 i64
       ])
    , Assignment (LMNLocalVar "runtimeErrorPrintfRet" i32)
      ( Call StdCall (LMGlobalVar "printf"
                       (LMFunction printfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [LMNLocalVar "runtimeErrorPrintfStr" (LMPointer i8)]
        []
      )
    , Expr
      ( Call StdCall (LMGlobalVar "exit"
                       (LMFunction exitDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [LMLitVar $ LMIntLit 1 i32]
        []
      )
    , Unreachable
    ]
  ]


printIntDecl = LlvmFunctionDecl
  "printInt"
  Internal
  CC_Ccc
  LMVoid
  FixedArgs
  [(i32, [])]
  Nothing

printInt = LlvmFunction
  printIntDecl
  ["x"]
  []
  Nothing
  Nothing
  [LlvmBlock (unique "printInt_enter")
    [ Assignment (LMNLocalVar "printIntPrintfStr" (LMPointer i8))
      (GetElemPtr True (getGlobalVar printIntStr)
       [ LMLitVar $ LMIntLit 0 i64
       , LMLitVar $ LMIntLit 0 i64
       ])
    , Assignment (LMNLocalVar "printIntPrintfRet" i32)
      ( Call StdCall (LMGlobalVar "printf"
                       (LMFunction printfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [ LMNLocalVar "printIntPrintfStr" (LMPointer i8)
        , LMNLocalVar "x" i32
        ]
        []
      )
    , Return Nothing
    ]
  ]


readIntDecl = LlvmFunctionDecl
  "readInt"
  Internal
  CC_Ccc
  i32
  FixedArgs
  []
  Nothing

readInt = LlvmFunction
  readIntDecl
  []
  []
  Nothing
  Nothing
  [LlvmBlock (unique "readInt_enter")
    [ Assignment (LMNLocalVar "readIntScanfStr" (LMPointer i8))
      (GetElemPtr True (getGlobalVar readIntStr)
       [ LMLitVar $ LMIntLit 0 i64
       , LMLitVar $ LMIntLit 0 i64
       ])
    , Assignment (LMNLocalVar "readIntScanfBuf" (LMPointer i32))
      (Alloca i32 1)
    , Assignment (LMNLocalVar "readIntScanfRet" i32)
      ( Call StdCall (LMGlobalVar "scanf"
                       (LMFunction scanfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [ LMNLocalVar "readIntScanfStr" (LMPointer i8)
        , LMNLocalVar "readIntScanfBuf" (LMPointer i32)
        ]
        []
      )
    , Assignment (LMNLocalVar "readIntScanfRes" i32)
      (Load (LMNLocalVar "readIntScanfBuf" (LMPointer i32)))
    , Return (Just $ LMNLocalVar "readIntScanfRes" i32)
    ]
  ]


printStringDecl = LlvmFunctionDecl
  "printString"
  Internal
  CC_Ccc
  LMVoid
  FixedArgs
  [(LMPointer i8, [])]
  Nothing

printString = LlvmFunction
  printStringDecl
  ["s"]
  []
  Nothing
  Nothing
  [LlvmBlock (unique "printString_enter")
    [ Assignment (LMNLocalVar "printStringPrintfRet" i32)
      ( Call StdCall (LMGlobalVar "printf"
                       (LMFunction printfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [ LMNLocalVar "s" (LMPointer i8)
        ]
        []
      )
    , Return Nothing
    ]
  ]



stdglobals = [runtimeErrorStr, printIntStr, readIntStr]
stddecls = [printfDecl, exitDecl, scanfDecl, strlenDecl, strcatDecl, callocDecl]
stddefs = [runtimeError, printInt, readInt, printString]

stdfenv = M.fromList
  [ (Id "error", (TVoid, []))
  , (Id "printInt", (TVoid, [TInt]))
  , (Id "readInt", (TInt, []))
  , (Id "printString", (TVoid, [TString]))
  ]
