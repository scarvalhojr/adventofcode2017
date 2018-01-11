
import qualified Data.Map as M (Map, empty, fromList, insert, adjust, lookup,
  findWithDefault)

type Register = Char
type Value = Int
type Offset = Value
data OperType = Set | Add | Mul | Mod deriving (Eq, Show)

data Instruction = OperR OperType Register Register
                 | OperV OperType Register Value
                 | JgzRR Register Register
                 | JgzRV Register Value
                 | JgzVV Value Value
                 | Snd Register
                 | Rcv Register
                 deriving (Show)

type Program = M.Map Int Instruction
type RegisterTable = M.Map Register Value
data Execution = Execution RegisterTable Offset Value deriving Show

runProgram :: Program -> Maybe Value
runProgram = fetchAndExec (Execution M.empty 0 0)

fetchAndExec :: Execution -> Program -> Maybe Value
fetchAndExec exec@(Execution _ offset _) prog
  = case M.lookup offset prog of
      Nothing   -> Nothing
      Just inst -> execInstr exec prog inst

execInstr :: Execution -> Program -> Instruction -> Maybe Value
execInstr exec prog inst
  = case execute exec inst of
      (exec', Nothing) -> fetchAndExec exec' prog
      (exec', justval) -> justval

execute :: Execution -> Instruction -> (Execution, Maybe Value)
execute e@(Execution regs _ _) (OperR o r rv) = execute e (OperV o r v)
  where v = M.findWithDefault 0 rv regs

execute (Execution regs offset play) (OperV o r v) = (exec', Nothing)
  where exec' = (Execution regs' (offset + 1) play)
        regs' = M.insert r val regs
        curr  = M.findWithDefault 0 r regs
        val   = case o of Set -> v
                          Add -> curr + v
                          Mul -> curr * v
                          Mod -> curr `mod` v

execute e@(Execution regs _ _) (JgzRR rc ro) = execute e (JgzRV rc o)
  where o = M.findWithDefault 0 ro regs

execute e@(Execution regs _ _) (JgzRV rc o) = execute e (JgzVV c o)
  where c = M.findWithDefault 0 rc regs

execute (Execution regs offset play) (JgzVV c o)
  | c > 0     = (Execution regs (offset + o) play, Nothing)
  | otherwise = (Execution regs (offset + 1) play, Nothing)

execute (Execution regs offset _) (Snd r) = (exec', Nothing)
  where exec' = (Execution regs (offset + 1) play)
        play  = M.findWithDefault 0 r regs

execute (Execution regs offset play) (Rcv r)
  | value /= 0  = (exec', Just play)
  | otherwise   = (exec', Nothing)
  where exec' = (Execution regs (offset + 1) play)
        value = M.findWithDefault 0 r regs

---

main :: IO ()
main = do
  let prog = compile instructions
  putStrLn $ "Part 1: " ++ show (runProgram prog)

compile :: [Instruction] -> Program
compile = M.fromList . zip [0..]

instructions = [OperV Set 'i' 31, OperV Set 'a' 1, OperV Mul 'p' 17,
                JgzRR 'p' 'p', OperV Mul 'a' 2, OperV Add 'i' (-1),
                JgzRV 'i' (-2), OperV Add 'a' (-1), OperV Set 'i' 127,
                OperV Set 'p' 316, OperV Mul 'p' 8505, OperR Mod 'p' 'a',
                OperV Mul 'p' 129749, OperV Add 'p' 12345, OperR Mod 'p' 'a',
                OperR Set 'b' 'p', OperV Mod 'b' 10000, Snd 'b',
                OperV Add 'i' (-1), JgzRV 'i' (-9), JgzRV 'a' 3, Rcv 'b',
                JgzRV 'b' (-1), OperV Set 'f' 0, OperV Set 'i' 126, Rcv 'a',
                Rcv 'b', OperR Set 'p' 'a', OperV Mul 'p' (-1),
                OperR Add 'p' 'b', JgzRV 'p' 4, Snd 'a', OperR Set 'a' 'b',
                JgzVV 1 3, Snd 'b', OperV Set 'f' 1, OperV Add 'i' (-1),
                JgzRV 'i' (-11), Snd 'a', JgzRV 'f' (-16), JgzRV 'a' (-19)]
