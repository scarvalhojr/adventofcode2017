
import qualified Data.Map as M (Map, fromList, findWithDefault)
import           Data.Maybe    (catMaybes)

data Cell = Empty | Horizontal | Vertical | Corner | Letter Char
  deriving (Eq, Show)

type Position = (Int, Int)
type Grid = M.Map Position Cell

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Eq, Show)

data Path = Path { len :: Int
                 , path :: [Char] }
  deriving (Eq, Show)

---

followPath :: Grid -> Position -> Direction -> Path
followPath grid pos dir = consumeCell grid next dir cell
  where next = move pos dir
        cell = M.findWithDefault Empty next grid

consumeCell :: Grid -> Position -> Direction -> Cell -> Path
consumeCell _ _ _ Empty = Path 1 []
consumeCell grid pos dir Corner
  | null paths       = Path 1 []
  | length paths > 1 = error "multiple paths"
  | otherwise        = addCell Corner (head paths)
  where paths = filter (not . empty) (map (followPath grid pos) next)
        next  = [d | d <- [DirUp, DirDown, DirLeft, DirRight],
                     d /= opposite dir]
consumeCell grid pos dir cell = addCell cell (followPath grid pos dir)

addCell :: Cell -> Path -> Path
addCell (Letter c) (Path len letters) = Path (len + 1) (c : letters)
addCell _          (Path len letters) = Path (len + 1) letters

empty :: Path -> Bool
empty (Path _ path) = null path

opposite :: Direction -> Direction
opposite DirUp    = DirDown
opposite DirDown  = DirUp
opposite DirLeft  = DirRight
opposite DirRight = DirLeft

move :: Position -> Direction -> Position
move (r, c) DirUp    = (r - 1,   c  )
move (r, c) DirDown  = (r + 1,   c  )
move (r, c) DirLeft  = (  r  , c - 1)
move (r, c) DirRight = (  r  , c + 1)

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let grid = processInput input
  let pos = (0,147)
  let dir = DirDown
  let solution = followPath grid pos dir
  putStrLn $ "Part 1: " ++ show (path solution)
  putStrLn $ "Part 2: " ++ show (len solution)

processInput :: String -> Grid
processInput = M.fromList . concat . map parseLine . zip [0..] . lines

parseLine :: (Int, String) -> [(Position, Cell)]
parseLine (row, line) = catMaybes $ map (parseCell row) (zip [0..] line)

parseCell :: Int -> (Int, Char) -> Maybe (Position, Cell)
parseCell  _  ( _ , ' ') = Nothing
parseCell row (col, ch ) = Just ((row, col), cell ch)
  where cell '-' = Horizontal
        cell '|' = Vertical
        cell '+' = Corner
        cell  c  = Letter c

---

sample :: IO ()
sample = do
  let inputFile = "sample.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let grid = processInput input
  let pos = (0,5)
  let dir = DirDown
  let solution = followPath grid pos dir
  putStrLn $ "Part 1: " ++ show (path solution)
  putStrLn $ "Part 2: " ++ show (len solution)
