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

vmError :: String -> a
vmError = error . ("VM Runtime Error: "++)

exeIns :: Memory -> Instruction -> IO Memory
exeIns (s, v)         (Const n) = return (n:s, v)
exeIns (I b:I a:s, v) Add       = return (I (a + b):s, v)
exeIns (b:a:s, v)     Add       = return (S (repr a ++ repr b):s, v)
exeIns (b:a:s, v)     Flip      = return (a:b:s, v)
exeIns (n:s, v)       Print     = putStr (repr n) >> return (s, v)
exeIns (n:s, v)       (Store i) = return (s, M.insert i n v)
exeIns (s, v)         (Load i)  = case M.lookup i v of
                                    (Just n) -> return (n:s, v)
                                    Nothing -> vmError $ "Attempted to access undefined variable " ++ i

exeCode :: Memory -> Bytecode -> IO Memory
exeCode = foldM exeIns

initialMem :: Memory
initialMem = ([], M.empty)

{-
Util function.
Takes two variable names and swaps their values.
-}
swap :: String -> String -> Bytecode
swap a b = [
  Load a,
  Load b,
  Store a,
  Store b]

main :: IO ()
main = do
  putStrLn "Executing..."
  mem <- exeCode initialMem example1
  putStrLn "Done."
  putStrLn "Stack memory:"
  print $ fst mem
  putStrLn "Variable memory:"
  print $ snd mem

example1 :: Bytecode
example1 = [
  -- print("I got ")
  Const $ S "I got ",
  Print,
  -- answer = 10 + 32
  Const $ I 10,
  Const $ I 32,
  Add,
  Store "answer",
  -- print(57 + answer)
  Const $ I 57,
  Load "answer",
  Add,
  Print,
  -- print(" problems\n")
  Const $ S " problems\n",
  Print]

example2 :: Bytecode
example2 = [
  -- num_problems = 99
  Const $ I 99,
  Store "num_problems",
  -- print("I got " + num_problems + " problems\n")
  Const $ S "I got ",
  Load "num_problems",
  Add,
  Const $ S " problems\n",
  Add,
  Print]
