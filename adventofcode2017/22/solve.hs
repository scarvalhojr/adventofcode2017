
import qualified Data.Map as M (Map, fromList, findWithDefault, delete, insert)

type Position = (Int, Int)

data NodeState = Clean | Weakened | Infected | Flagged
  deriving (Eq, Show)

type InfectedMap = M.Map Position NodeState

data Direction = DirUp | DirRight | DirDown | DirLeft
  deriving (Eq, Enum, Show)

type Counter = Integer

data VirusState = VirusState InfectedMap Position Direction Counter
  deriving Show

solve1 :: InfectedMap -> Int -> Counter
solve1 = countInfections virus1

countInfections :: (VirusState -> VirusState) -> InfectedMap -> Int -> Counter
countInfections virus map iter = count
  where state = VirusState map (0, 0) DirUp 0
        final = last $ take (iter + 1) (iterate virus state)
        VirusState _ _ _ count = final

virus1 :: VirusState -> VirusState
virus1 state@(VirusState map pos dir _)
  | node == Infected   = clean state (turnRight dir)
  | otherwise          = infect state (turnLeft dir)
  -- ^ Clean
  where node = M.findWithDefault Clean pos map

clean :: VirusState -> Direction -> VirusState
clean (VirusState map pos _ count) dir = VirusState map' pos' dir count
  where map' = M.delete pos map
        pos' = move pos dir

infect :: VirusState -> Direction -> VirusState
infect (VirusState map pos _ count) dir = VirusState map' pos' dir (count + 1)
  where map' = M.insert pos Infected map
        pos' = move pos dir

turnLeft :: Direction -> Direction
turnLeft DirUp = DirLeft
turnLeft dir   = pred dir

turnRight :: Direction -> Direction
turnRight DirLeft = DirUp
turnRight dir     = succ dir

move :: Position -> Direction -> Position
move (row, col) DirUp    = (row - 1, col)
move (row, col) DirDown  = (row + 1, col)
move (row, col) DirLeft  = (row, col - 1)
move (row, col) DirRight = (row, col + 1)

---

solve2 :: InfectedMap -> Int -> Counter
solve2 = countInfections virus2

virus2 :: VirusState -> VirusState
virus2 state@(VirusState map pos dir _)
  | node == Clean      = update state Weakened (turnLeft dir)
  | node == Weakened   = update state Infected dir
  | node == Infected   = update state Flagged (turnRight dir)
  | otherwise          = clean state (reverseDir dir)
  -- ^ Flagged
  where node = M.findWithDefault Clean pos map

update :: VirusState -> NodeState -> Direction -> VirusState
update (VirusState map pos _ count) node dir
  | node == Infected   = VirusState map' pos' dir (count + 1)
  | otherwise          = VirusState map' pos' dir count
  where map' = M.insert pos node map
        pos' = move pos dir

reverseDir :: Direction -> Direction
reverseDir DirUp    = DirDown
reverseDir DirRight = DirLeft
reverseDir DirDown  = DirUp
reverseDir DirLeft  = DirRight

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let map = processInput input
  let iter1 = 10000
  putStrLn $ "Part 1: " ++ show (solve1 map iter1)
  let iter2 = 10000000
  putStrLn $ "Part 2: " ++ show (solve2 map iter2)

processInput :: String -> InfectedMap
processInput = M.fromList . filterInfect . mapPosition . map parseState . lines
  where filterInfect = filter ((== Infected) . snd)

parseState :: [Char] -> [NodeState]
parseState []       = []
parseState ('.':xs) = Clean : parseState xs
parseState ('#':xs) = Infected : parseState xs
parseState ( _ :xs) = parseState xs

mapPosition :: [[NodeState]] -> [(Position, NodeState)]
mapPosition input = (concat . map row . zip [start input..]) input
  where start lst    = - (length lst `div` 2)
        row (r, xs)  = map (pos r) (zip [start xs..] xs)
        pos r (c, x) = ((r, c), x)

---

sample :: IO ()
sample = do
  let inputFile = "sample.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let map = processInput input
  let iter1 = 10000
  putStrLn $ "Part 1: " ++ show (solve1 map iter1)
  let iter2 = 10000000
  putStrLn $ "Part 2: " ++ show (solve2 map iter2)
