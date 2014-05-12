module VM where

import Control.Monad (foldM)
import qualified Data.Map as M

data Value
  = S String
  | I Int
  deriving (Eq, Show)

data Instruction
  = Const Value
  | Add
  | Flip
  | Print
  | Store String
  | Load String
  deriving (Eq, Show)

type Bytecode = [Instruction]
type Stack = [Value]
type Vars = M.Map String Value
type Memory = (Stack, Vars)

repr :: Value -> String
repr (S s) = s
repr (I n) = show n

exeIns :: Memory -> Instruction -> IO Memory
exeIns (s, v)         (Const n) = return (n:s, v)
exeIns (I b:I a:s, v) Add       = return (I (a + b):s, v)
exeIns (b:a:s, v)     Add       = return (S (repr a ++ repr b):s, v)
exeIns (b:a:s, v)     Flip      = return (a:b:s, v)
exeIns (n:s, v)       Print     = putStr (repr n) >> return (s, v)
exeIns (n:s, v)       (Store i) = return (s, M.insert i n v)
exeIns (s, v)         (Load i)  = return (v M.! i:s, v)

exeCode :: Memory -> Bytecode -> IO Memory
exeCode = foldM exeIns

initialMem :: Memory
initialMem = ([], M.empty)
