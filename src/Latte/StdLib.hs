module Latte.StdLib(stdglobals, stddecls, stddefs, stdfenv) where

import Latte.Types.Latte

import Llvm
import FastString
import Unique

import qualified Data.Map as M

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

runtimeErrorStr = LMGlobal
  { getGlobalVar = LMGlobalVar "error_msg"
                   (LMPointer (LMArray 15 i8))
                   Internal
                   Nothing
                   (Just 1)
                   Constant
  , getGlobalValue = Just $ LMStaticStr
                     (fsLit "runtime error\\0A")
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
                     (fsLit "%d\\0A")
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
                     (fsLit "%d")
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
  [LlvmBlock (getUnique $ fsLit "runtimeError_enter")
    [ Assignment (LMNLocalVar (fsLit "runtimeErrorPrintfStr") (LMPointer i8))
      (GetElemPtr True (getGlobalVar runtimeErrorStr)
       [ LMLitVar $ LMIntLit 0 i64
       , LMLitVar $ LMIntLit 0 i64
       ])
    , Assignment (LMNLocalVar (fsLit "runtimeErrorPrintfRet") i32)
      ( Call StdCall (LMGlobalVar (fsLit "printf")
                       (LMFunction printfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [LMNLocalVar (fsLit "runtimeErrorPrintfStr") (LMPointer i8)]
        []
      )
    , Expr
      ( Call StdCall (LMGlobalVar (fsLit "exit")
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
  [fsLit "x"]
  []
  Nothing
  Nothing
  [LlvmBlock (getUnique $ fsLit "printInt_enter")
    [ Assignment (LMNLocalVar (fsLit "printIntPrintfStr") (LMPointer i8))
      (GetElemPtr True (getGlobalVar printIntStr)
       [ LMLitVar $ LMIntLit 0 i64
       , LMLitVar $ LMIntLit 0 i64
       ])
    , Assignment (LMNLocalVar (fsLit "printIntPrintfRet") i32)
      ( Call StdCall (LMGlobalVar (fsLit "printf")
                       (LMFunction printfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [ LMNLocalVar (fsLit "printIntPrintfStr") (LMPointer i8)
        , LMNLocalVar (fsLit "x") i32
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
  [LlvmBlock (getUnique $ fsLit "readInt_enter")
    [ Assignment (LMNLocalVar (fsLit "readIntScanfStr") (LMPointer i8))
      (GetElemPtr True (getGlobalVar readIntStr)
       [ LMLitVar $ LMIntLit 0 i64
       , LMLitVar $ LMIntLit 0 i64
       ])
    , Assignment (LMNLocalVar (fsLit "readIntScanfBuf") (LMPointer i32))
      (Alloca i32 1)
    , Assignment (LMNLocalVar (fsLit "readIntScanfRet") i32)
      ( Call StdCall (LMGlobalVar (fsLit "scanf")
                       (LMFunction scanfDecl)
                       Internal
                       Nothing
                       Nothing
                       Constant
                     )
        [ LMNLocalVar (fsLit "readIntScanfStr") (LMPointer i8)
        , LMNLocalVar (fsLit "readIntScanfBuf") (LMPointer i32)
        ]
        []
      )
    , Assignment (LMNLocalVar (fsLit "readIntScanfRes") i32)
      (Load (LMNLocalVar (fsLit "readIntScanfBuf") (LMPointer i32)))
    , Return (Just $ LMNLocalVar (fsLit "readIntScanfRes") i32)
    ]
  ]

stdglobals = [runtimeErrorStr, printIntStr, readIntStr]
stddecls = [printfDecl, exitDecl, scanfDecl]
stddefs = [runtimeError, printInt, readInt]

stdfenv = M.fromList
  [ (Id "error", (TVoid, []))
  , (Id "printInt", (TVoid, [TInt]))
  , (Id "readInt", (TInt, []))
  ]
