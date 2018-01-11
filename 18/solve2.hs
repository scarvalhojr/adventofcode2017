
import           Data.List     (partition)
import qualified Data.Map as M (Map, fromList, insert, adjust, lookup,
                                findWithDefault)

type Register = Char
type Value = Int
type RegisterTable = M.Map Register Value

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

type Offset = Value
type OutMsg = Value
type InMsgs = [Value]
data ProgramState = Running | Sending | Receiving | Terminated | Error
  deriving (Eq, Show)
data Execution = Execution ProgramState RegisterTable Offset OutMsg InMsgs
  deriving Show

type ProcessID = Int
type ReceiverID = ProcessID
type SendCounter = Int
data Process = Process ProcessID Execution SendCounter ReceiverID
  deriving Show

type ProcessTable = [Process]

solve2 :: Program -> SendCounter
solve2 prog = getSendCounter proc1
  where procs  = runProcesses prog [createProcess 0 1, createProcess 1 0]
        proc1 = head $ filter ((==1). getPid) procs

getSendCounter :: Process -> SendCounter
getSendCounter (Process _ _ counter _) = counter

createProcess :: ProcessID -> ReceiverID -> Process
createProcess pid recid = Process pid exec 0 recid
  where exec = Execution Running (M.fromList [('p', pid)]) 0 0 []

runProcesses :: Program -> ProcessTable -> ProcessTable
runProcesses _ [] = []
runProcesses prog procs
  = case schedule procs of
      (Nothing, _)     -> procs
      (Just p, others) -> runProcesses prog (handleProcess prog p others)

schedule :: ProcessTable -> (Maybe Process, ProcessTable)
schedule [] = (Nothing, [])
schedule procs
  | null unblocked  = (Nothing, procs)
  | otherwise       = (Just (head unblocked), tail unblocked ++ blocked)
  where (unblocked, blocked) = partition isUnblocked procs
        isUnblocked = (`elem` [Running, Sending]) . getState

getState :: Process -> ProgramState
getState (Process _ (Execution state _ _ _ _) _ _) = state

handleProcess :: Program -> Process -> ProcessTable -> ProcessTable
handleProcess prog p@(Process pid exec count recid) procs
  | state == Running  = (Process pid (runProgram prog exec) count recid) : procs
  | state == Sending  = (handleMessage p receivers) ++ others
  | otherwise         = p : procs
  where state = getState p
        (receivers, others) = partition ((== recid) . getPid) procs

getPid :: Process -> ProcessID
getPid (Process pid _ _ _) = pid

handleMessage :: Process -> [Process] -> [Process]
handleMessage sender [receiver] = [sender', receiver']
  where (message, sender') = extractMessage sender
        receiver' = receiveMessage receiver message
handleMessage (Process pid (Execution _ regs offset outm inms) count rid) rs
  = (Process pid (Execution Error regs offset outm inms) count rid) : rs

extractMessage :: Process -> (Value, Process)
extractMessage (Process pid (Execution _ regs offset outm inms) count recid)
  = (outm,
     Process pid (Execution Running regs offset outm inms) (count + 1) recid)

receiveMessage :: Process -> Value -> Process
receiveMessage (Process pid (Execution _ regs offset outm inms) count recid) msg
  = Process pid (Execution Running regs offset outm (inms ++ [msg])) count recid

runProgram :: Program -> Execution -> Execution
runProgram prog exec@(Execution Running regs offset outmsg inmsgs)
  = case M.lookup offset prog of
      Nothing   -> Execution Terminated regs offset outmsg inmsgs
      Just inst -> runProgram prog (execute exec inst)
runProgram _ exec = exec

execute :: Execution -> Instruction -> Execution
execute e@(Execution _ regs _ _ _) (OperR o r rv) = execute e (OperV o r v)
  where v = M.findWithDefault 0 rv regs

execute (Execution state regs offset outmsg inmsgs) (OperV o r v) = exec'
  where exec' = (Execution state regs' (offset + 1) outmsg inmsgs)
        regs' = M.insert r val regs
        curr  = M.findWithDefault 0 r regs
        val   = case o of Set -> v
                          Add -> curr + v
                          Mul -> curr * v
                          Mod -> curr `mod` v

execute e@(Execution _ regs _ _ _) (JgzRR rc ro) = execute e (JgzRV rc o)
  where o = M.findWithDefault 0 ro regs

execute e@(Execution _ regs _ _ _) (JgzRV rc o) = execute e (JgzVV c o)
  where c = M.findWithDefault 0 rc regs

execute (Execution state regs offset outmsg inmsgs) (JgzVV c o)
  | c > 0     = Execution state regs (offset + o) outmsg inmsgs
  | otherwise = Execution state regs (offset + 1) outmsg inmsgs

execute (Execution state regs offset _ inmsgs) (Snd r) = exec'
  where exec'  = Execution Sending regs (offset + 1) outmsg inmsgs
        outmsg = M.findWithDefault 0 r regs

execute (Execution state regs offset outmsg []) (Rcv r) = exec'
  where exec' = Execution Receiving regs offset outmsg []

execute (Execution state regs offset outmsg (x:xs)) (Rcv r) = exec'
  where exec' = Execution state regs' (offset + 1) outmsg xs
        regs' = M.insert r x regs

---

main :: IO ()
main = do
  let prog = compile instructions
  putStrLn $ "Part 2: " ++ show (solve2 prog)

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
