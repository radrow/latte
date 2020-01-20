{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Latte.Backend.X86.Compile where

import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.Char
import Data.List hiding (or, and)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS.Lazy hiding ((<>))
import Control.Monad.State
import Prelude hiding ((<>), or, and)

import qualified Latte.Frontend.IR as IR
import qualified Latte.Frontend.AST as AST
import Latte.Backend.X86.X86 as X86
import Latte.Backend.X86.PeepHole
import Latte.Pretty

type VarMap = M.Map IR.VarId Operand

data Env = Env
  { _envVarMap :: VarMap
  }
data St = St
  { _stSupply :: Int
  , _stStrings :: M.Map String String
  }

type Compiler = RWS Env [Instr] St

makeLensesWith abbreviatedFields ''Env
makeLensesWith abbreviatedFields ''St

getLoc :: IR.VarId -> Compiler Operand
getLoc v = views varMap (M.! v)

nextSup :: Compiler Int
nextSup = do
  i <- use supply
  modify $ over supply (+1)
  return i

makeLabel :: String -> Compiler String
makeLabel s = do
  i <- nextSup
  let l = "__" ++ s ++ show i
  label l
  return l

makeStrName :: String -> Compiler String
makeStrName s = do
  i <- nextSup
  return $ "__local_str_" ++ filter isAlphaNum s ++ "_" ++ show i

requestStr :: String -> Compiler String
requestStr s = do
  uses strings (M.lookup s) >>= \case
    Nothing -> do
      v <- makeStrName s
      modify $ over strings (M.insert s v)
      return v
    Just v -> return v

cConst :: IR.Const -> Operand -> Compiler ()
cConst ic o = case ic of
  IR.CVar c | isMem o -> do
    cloc <- getLoc c
    mov cloc edx
    mov edx o
  IR.CVar c -> do
    cloc <- getLoc c
    mov cloc o
  IR.CInt i -> do
    mov (OConst i) o
  IR.CStr s -> do
    sn <- requestStr s
    if isReg o
      then mov (OVar sn) o
      else do
      mov (OVar sn) edx
      mov edx o

cNOp :: IR.NumOp -> Operand -> Operand -> Compiler ()
cNOp = \case
  IR.Add -> add
  IR.Sub -> sub
  IR.Mul -> \a b -> do
    mov b eax
    mov a ecx
    cdq
    imul ecx
    mov eax b
  IR.Div -> \a b -> do
    mov b eax
    mov a ecx
    cdq
    idiv ecx
    mov eax b
  IR.Mod -> \a b -> do
    mov b eax
    mov a ecx
    cdq
    idiv ecx
    mov edx b
  IR.And -> and
  IR.Or  -> or

cROp :: IR.RelOp -> Operand -> Operand -> Compiler ()
cROp o l r = do
  -- let i = case o of
  --       IR.Eq  -> sete
  --       IR.Neq  -> setne
  --       IR.Gt  -> setg
  --       IR.Ge  -> setge
  --       IR.Lt  -> setl
  --       IR.Le  -> setle
  let j = case o of
        IR.Eq  -> je
        IR.Neq  -> jne
        IR.Gt  -> jg
        IR.Ge  -> jge
        IR.Lt  -> jl
        IR.Le  -> jle
  push r
  push l
  cmp l r
  void $ mfix $ \con -> do
    void $ mfix $ \vtrue -> do
      j $ OLabel vtrue
      mov (OConst 0) r
      jmp $ OLabel con
      makeLabel "boolean_t"
    mov (OConst 1) r
    makeLabel "boolean_con"

cROpJmp :: IR.RelOp -> String -> String -> Compiler ()
cROpJmp o ltrue lfalse = do
  let j = case o of
        IR.Eq  -> je
        IR.Neq  -> jne
        IR.Gt  -> jg
        IR.Ge  -> jge
        IR.Lt  -> jl
        IR.Le  -> jle
  j $ OLabel ltrue
  jmp $ OLabel lfalse

sizeOf :: IR.Type -> Compiler Int
sizeOf = \case
  IR.TObj _ flds -> return $ (length flds + 1) * 4
  _ -> return 4

makeMethodDispatcherName :: String -> String
makeMethodDispatcherName s = "__" ++ s ++ "_dispatch"

makeMethodVTableName :: String -> String
makeMethodVTableName s = "__" ++ s ++ "_vtable"

cExpr :: IR.Type -> Operand -> IR.Expr -> Compiler ()
cExpr t vloc e = case e of
  IR.Const c ->
    cConst c vloc
  IR.NumOp o l r -> do
    cConst l eax
    cConst r ecx
    cNOp o ecx eax
    mov eax vloc
  IR.RelOp o l r -> do
    cConst l eax
    cConst r ecx
    cROp o ecx eax
    mov eax vloc
  IR.UnOp o v -> do
    cConst v eax
    case o of
      IR.Not -> do
        xor ecx ecx
        test eax eax
        setz cl
        mov ecx eax
      IR.Neg ->
        neg eax
    mov eax vloc
  IR.Call f as -> do
    forM_ (reverse as) $ \a -> do
      cConst a eax
      push eax
    call (OLabel f)
    add (OConst $ 4 * fromIntegral (length as)) esp
    mov eax vloc
  IR.Proj v off -> do
    cConst v eax
    mov (OConst $ fromIntegral off) edx
    mov (mem (0 :: Int) (EAX, EDX, 4 :: Int)) eax
    mov eax vloc
  IR.NewObj -> do
    s <- sizeOf t
    let IR.TObj idx _ = t
    push (OConst $ toInteger s)
    call (OLabel "malloc")
    add (OConst 4) esp
    mov (OConst $ fromIntegral idx) (mem EAX)
    mov eax vloc
  IR.VCall f cid obj as -> do
    forM_ (reverse as) $ \a -> do
      cConst a eax
      push eax
    cConst obj eax
    push eax
    cConst cid eax
    push eax
    call (OLabel $ makeMethodDispatcherName f)
    add (OConst $ 4 * fromIntegral (length as + 1)) esp
    mov eax vloc
  IR.GetCId c -> do
    cConst c eax
    mov (mem EAX) eax
    mov eax vloc

cInstr :: IR.Instr -> Compiler ()
cInstr i = comment (pp i) >> case i of
  IR.Assg t loc e -> do
    vloc <- getLoc loc
    cExpr t vloc e
  IR.FieldAssg t b f e -> do
    cConst b ecx
    push ecx
    cExpr t eax e
    pop ecx
    mov (OConst $ fromIntegral f) edx
    mov eax (mem (0 :: Int) (ECX, EDX, 4 :: Int))

cFinInstr :: IR.FinInstr -> Compiler ()
cFinInstr i = comment (pp i) >> case i of
  IR.Ret Nothing -> do
    leave
    ret
  IR.Ret (Just v) -> do
    cConst v eax
    leave
    ret
  IR.Jmp (IR.Label l) -> jmp (OLabel l)
  IR.Br c (IR.Label ltrue) (IR.Label lfalse) -> case c of
    IR.CondConst cc -> do
      cConst cc eax
      cmp (OConst 0) eax
      jne $ OLabel ltrue
      jmp $ OLabel lfalse
    IR.Cond o l r -> do
      cConst l eax
      cConst r ecx
      cmp ecx eax
      cROpJmp o ltrue lfalse
  IR.TailCall f as -> do
    forM_ (reverse $ zip [8,12..] as) $ \(idx, a) -> do
      cConst a eax
      mov eax (mem (idx :: Int) EBP)
    jmp (OLabel $ f ++ "_init")
  IR.TailVCall _f _as -> error "Tail virtual calls are not supported yet"
  IR.Unreachable ->
    call (OLabel "__unreachable")

cBlock :: IR.Block -> Compiler ()
cBlock (IR.Block (IR.Label bname) instrs fin) = do
  label bname
  forM_ instrs cInstr
  cFinInstr fin

cRoutine :: IR.Routine -> Compiler ()
cRoutine (IR.Routine virt (IR.Label rname) args blocks) = do
  globl rname
  let locals = S.unions (map IR.getUsedVars blocks) S.\\ S.fromList args
      routineVarEnv = M.fromList $ localSupply ++ argSupply where
        localSupply = flip evalState 1 $
          mapM (\v -> do
                   i <- get
                   put (i+1)
                   return (v, mem (-(4::Int) * i) EBP :: Operand)) (S.toList locals)
        argSupply = flip evalState 2 $
          mapM (\v -> do
                   i <- get
                   put (i+1)
                   return (v, mem ((4::Int) * i) EBP :: Operand)) args
  local (set varMap routineVarEnv) $ do
    label rname
    when (Prelude.not virt) $ do
      push ebp
      mov esp ebp
    sub (OConst $ fromIntegral $ length locals * 4) esp
    forM_ blocks cBlock


cVirtualMethodTables :: IR.MethodMap -> Compiler ()
cVirtualMethodTables methodMap = do
  let size = maximum $ [c | (_, implMap) <- methodMap, cls <- M.elems implMap, c <- cls]
  forM_ methodMap $ \(mname, implMap) -> do
    let vtableName = makeMethodVTableName $ mname^.AST.idStr
        dispatcherName = makeMethodDispatcherName $ mname^.AST.idStr
        table :: [String]
        table = runST $ do
          a <- newArray (0, size) "__virtual_method_fail" :: ST s (STArray s Int String)
          forM_ (M.toList implMap) $ \(impl, ids) -> do
            forM_ ids $ \i -> do
              writeArray a (i :: Int) impl
          af <- freeze a
          return (elems af)
    label vtableName
    forM_ table $ \e -> do
      long e
    label dispatcherName

    pop edx
    pop ecx
    push edx

    push ebp
    mov esp ebp

    lea (OLabel vtableName) edx
    mov (mem (EDX, ECX, 4 :: Int)) edx

    jmp edx


compileStrings :: M.Map String String -> Compiler ()
compileStrings strs = forM_ (M.toList strs) $ \(s, v) -> do
  sup <- nextSup
  let l = ".LC" ++ show sup
  label l
  string s
  label v
  long l

cProgram :: IR.IR -> Compiler ()
cProgram (IR.IR routines) = do
  forM_ routines cRoutine
  use strings >>= compileStrings
  return ()

compile :: IR.MethodMap -> IR.IR -> Assembly
compile mm ir = optimize $
  let act = do
        cProgram ir
        cVirtualMethodTables mm
  in Assembly $ snd $ evalRWS act undefined (St 1 M.empty)
