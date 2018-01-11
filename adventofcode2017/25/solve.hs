
import qualified Data.Map as M (Map, lookup, fromList)
import qualified Data.Set as S (Set, member, empty, size, insert, delete)

type Position = Int

data Value = Zero | One
  deriving (Eq, Ord, Show)

type Tape = S.Set Position

data Move = MoveLeft | MoveRight
  deriving (Eq, Show)

type State = Char

data Action = Action Value Move State
  deriving Show

data Machine = Machine Tape State Position
  deriving Show

type Rules = M.Map (State, Value) Action

checksum :: Machine -> Int
checksum (Machine tape _ _) = S.size tape

run :: Rules -> Int -> Machine
run rules times = last $ take (times + 1) (iterate (compute rules) start)
  where start = Machine S.empty 'A' 0

compute :: Rules -> Machine -> Machine
compute rules machine = case fetch machine rules of
                          Nothing  -> machine
                          Just act -> execute machine act

fetch :: Machine -> Rules -> Maybe Action
fetch (Machine tape state pos) = M.lookup (state, value)
  where value = case S.member pos tape of
                  True  -> One
                  False -> Zero

execute :: Machine -> Action -> Machine
execute (Machine tape _ pos) (Action val move state) = Machine tape' state pos'
  where tape' = case val of
                  Zero -> S.delete pos tape
                  One  -> S.insert pos tape
        pos'  = case move of
                  MoveLeft  -> pos - 1
                  MoveRight -> pos + 1

---
steps :: Int
steps = 12261543

rules :: Rules
rules = M.fromList [(('A', Zero), (Action One  MoveRight 'B'))
                   ,(('A', One ), (Action Zero MoveLeft  'C'))
                   ,(('B', Zero), (Action One  MoveLeft  'A'))
                   ,(('B', One ), (Action One  MoveRight 'C'))
                   ,(('C', Zero), (Action One  MoveRight 'A'))
                   ,(('C', One ), (Action Zero MoveLeft  'D'))
                   ,(('D', Zero), (Action One  MoveLeft  'E'))
                   ,(('D', One ), (Action One  MoveLeft  'C'))
                   ,(('E', Zero), (Action One  MoveRight 'F'))
                   ,(('E', One ), (Action One  MoveRight 'A'))
                   ,(('F', Zero), (Action One  MoveRight 'A'))
                   ,(('F', One ), (Action One  MoveRight 'E'))]

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (checksum (run rules steps))

---

sample :: Rules
sample = M.fromList [(('A', Zero), (Action One  MoveRight 'B'))
                    ,(('A', One ), (Action Zero MoveLeft  'B'))
                    ,(('B', Zero), (Action One  MoveLeft  'A'))
                    ,(('B', One ), (Action One  MoveRight 'A'))]
