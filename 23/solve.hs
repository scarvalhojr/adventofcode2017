
import qualified Data.Map as M (Map, fromList, lookup, empty, findWithDefault
                               , insert)

type Register = Char
type Value = Int
type RegisterTable = M.Map Register Value

data Operand = OperValue Value | OperReg Register
  deriving (Eq, Show)

data Instruction = Set Register Operand
                 | Sub Register Operand
                 | Mul Register Operand
                 | Jnz Operand  Operand
                 deriving (Eq, Show)

type Program = M.Map Int Instruction

type Counter = Integer
type Offset = Value
data ProcessState = ProcessState RegisterTable Offset Counter
  deriving Show

runProgram1 :: Program -> ProcessState
runProgram1 = fetchAndExec state
  where state = ProcessState M.empty 0 0

fetchAndExec :: ProcessState -> Program -> ProcessState
fetchAndExec state@(ProcessState _ offset _) prog =
  case M.lookup offset prog of
    Nothing   -> state
    Just inst -> fetchAndExec (execute state inst) prog

execute :: ProcessState -> Instruction -> ProcessState
execute (ProcessState table offset count) (Set reg oper)
  = (ProcessState table' (offset + 1) count)
  where operand = operandValue table oper
        table'  = M.insert reg operand table

execute (ProcessState table offset count) (Sub reg oper)
  = (ProcessState table' (offset + 1) count)
  where value   = M.findWithDefault 0 reg table
        operand = operandValue table oper
        table'  = M.insert reg (value - operand) table

execute (ProcessState table offset count) (Mul reg oper)
  = (ProcessState table' (offset + 1) (count + 1))
  where value   = M.findWithDefault 0 reg table
        operand = operandValue table oper
        table'  = M.insert reg (value * operand) table

execute (ProcessState table offset count) (Jnz cond jump)
  | operandValue table cond /= 0  = (ProcessState table offset' count)
  | otherwise                     = (ProcessState table (offset + 1) count)
  where offset' = offset + (operandValue table jump)

operandValue :: RegisterTable -> Operand -> Value
operandValue _     (OperValue val) = val
operandValue table (OperReg   reg) = M.findWithDefault 0 reg table

---

runProgram2 :: Program -> ProcessState
runProgram2 = fetchAndExec state
  where state = ProcessState (M.fromList [('a', 1)]) 0 0

-- 501 too low
-- 918 wrong answer
-- 1000 too high
---

main :: IO ()
main = do
  --let prog1 = compile instructions1
  --putStrLn $ "Part 1: " ++ show (runProgram2 prog1)
  let prog2 = compile instructions2
  putStrLn $ "Part 2: " ++ show (runProgram2 prog2)

compile :: [Instruction] -> Program
compile = M.fromList . zip [0..]

instructions1 :: [Instruction]
instructions1 = [
  Set 'b' (OperValue 65),
  Set 'c' (OperReg 'b'),
  Jnz (OperReg 'a') (OperValue 2),
  Jnz (OperValue 1) (OperValue 5),
  Mul 'b' (OperValue 100),
  Sub 'b' (OperValue (-100000)),
  Set 'c' (OperReg 'b'),
  Sub 'c' (OperValue (-17000)),
  Set 'f' (OperValue 1),
  Set 'd' (OperValue 2),
  Set 'e' (OperValue 2),
  Set 'g' (OperReg 'd'),
  Mul 'g' (OperReg 'e'),
  Sub 'g' (OperReg 'b'),
  Jnz (OperReg 'g') (OperValue 2),
  Set 'f' (OperValue 0),
  Sub 'e' (OperValue (-1)),
  Set 'g' (OperReg 'e'),
  Sub 'g' (OperReg 'b'),
  Jnz (OperReg 'g') (OperValue (-8)),
  Sub 'd' (OperValue (-1)),
  Set 'g' (OperReg 'd'),
  Sub 'g' (OperReg 'b'),
  Jnz (OperReg 'g') (OperValue (-13)),
  Jnz (OperReg 'f') (OperValue 2),
  Sub 'h' (OperValue (-1)),
  Set 'g' (OperReg 'b'),
  Sub 'g' (OperReg 'c'),
  Jnz (OperReg 'g') (OperValue 2),
  Jnz (OperValue 1) (OperValue 3),
  Sub 'b' (OperValue (-17)),
  Jnz (OperValue 1) (OperValue (-23))]

instructions2 :: [Instruction]
instructions2 = [
  Set 'b' (OperValue 65),
  Set 'c' (OperReg 'b'),
  Jnz (OperReg 'a') (OperValue 2),
  Jnz (OperValue 1) (OperValue 5),
  Mul 'b' (OperValue 100),
  Sub 'b' (OperValue (-100000)),
  Set 'c' (OperReg 'b'),
  Sub 'c' (OperValue (-17)),            -- Adjusted value (was -17000)
  Set 'f' (OperValue 1),
  Set 'd' (OperValue 2),
  Set 'e' (OperValue 2),
  Set 'g' (OperReg 'd'),
  Mul 'g' (OperReg 'e'),
  Sub 'g' (OperReg 'b'),
  Jnz (OperReg 'g') (OperValue 3),       -- Adjusted jump (+1)
  Set 'f' (OperValue 0),
  Jnz (OperValue 1) (OperValue 9),       -- New jump
  Sub 'e' (OperValue (-1)),
  Set 'g' (OperReg 'e'),
  Sub 'g' (OperReg 'b'),
  Jnz (OperReg 'g') (OperValue (-9)),    -- Adjusted jump (-1)
  Sub 'd' (OperValue (-1)),
  Set 'g' (OperReg 'd'),
  Sub 'g' (OperReg 'b'),
  Jnz (OperReg 'g') (OperValue (-14)),   -- Adjusted jump (-1)
  Jnz (OperReg 'f') (OperValue 2),
  Sub 'h' (OperValue (-1)),
  Set 'g' (OperReg 'b'),
  Sub 'g' (OperReg 'c'),
  Jnz (OperReg 'g') (OperValue 2),
  Jnz (OperValue 1) (OperValue 3),
  Sub 'b' (OperValue (-17)),
  Jnz (OperValue 1) (OperValue (-24))]   -- Adjusted jump (-1)

isPrime k = null [ x | x <- [2..k `div` 2], k `mod` x  == 0]
