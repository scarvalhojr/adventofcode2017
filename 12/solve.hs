
import qualified Data.Map as M (Map, empty, insert, lookup, toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (nub)

type Program = Int

data PipeInfo = PipeInfo Program [Program]

type PipeMap = M.Map Program [Program]

solve1 :: PipeMap -> Int
solve1 = length . reachableFrom 0

reachableFrom :: Program -> PipeMap -> [Program]
reachableFrom prog = fst . reachAll ([prog], [prog])

reachAll :: ([Program], [Program]) -> PipeMap -> ([Program], [Program])
reachAll (reached, [])   _     = (reached, [])
reachAll (reached, todo) pipes = reachAll (reached ++ next, next) pipes
  where next = nub [n | p <- todo,
                        n <- fromMaybe [] (M.lookup p pipes),
                        not (n `elem` reached)]

processInput :: String -> PipeMap
processInput = foldr insert M.empty . catMaybes . map parseInput . lines
  where insert (PipeInfo source dests) = M.insert source dests

parseInput :: String -> Maybe PipeInfo
parseInput s
  | length progs <  2  = Nothing
  | otherwise          = info
  where progs   = (catMaybes . map parseInt . words . filter (/=',')) s
        info    = Just (PipeInfo (head progs) (tail progs))

parseInt :: String -> Maybe Int
parseInt s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing

---

solve2 :: PipeMap -> Int
solve2 = length . connectedGroups

connectedGroups :: PipeMap -> [[Program]]
connectedGroups pipes = snd $ groupAll (keys, []) pipes
  where keys = map fst $ M.toList pipes

groupAll :: ([Program], [[Program]]) -> PipeMap -> ([Program], [[Program]])
groupAll ([], groups)   _     = ([], groups)
groupAll (todo, groups) pipes = groupAll (remaining, newgroup : groups) pipes
  where start = head todo
        newgroup  = reachableFrom start pipes
        remaining = filter (not . (`elem` newgroup)) todo

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let info = processInput input
  putStrLn $ "Part 1: " ++ show (solve1 info)
  putStrLn $ "Part 2: " ++ show (solve2 info)
