
import qualified Data.Map as M (Map, empty, insert, lookup, findWithDefault)
import Data.Maybe (catMaybes)
import Data.List (sort, group)
import Data.Char (isDigit)

type Program = String
type Weight = Int
data ProgramInfo = ProgramInfo Program Weight [Program] deriving Show

type SupporterMap = M.Map Program Program

solve1 :: [ProgramInfo] -> Program
solve1 programs = case findBottom programs of
                    Nothing     -> ""
                    Just bottom -> bottom

processInput :: String -> [ProgramInfo]
processInput = catMaybes . map parseInputLine . lines

findBottom :: [ProgramInfo] -> Maybe Program
findBottom programs
  | null programs  = Nothing
  | otherwise      = follow anyprog
  where supmap = buildSupporterMap programs
        ProgramInfo anyprog _ _ = head programs
        follow prog = case M.lookup prog supmap of
                        Nothing  -> Just prog
                        Just sup -> follow sup

buildSupporterMap :: [ProgramInfo] -> SupporterMap
buildSupporterMap = foldr insert M.empty
  where insert (ProgramInfo prog _ supported) supmap =
          foldr ((flip M.insert) prog) supmap supported

parseInputLine :: String -> Maybe ProgramInfo
parseInputLine line
  | length tokens <  2  = Nothing
  | otherwise           = info
  where tokens    = words line
        prog      = head tokens
        weight    = parseInt $ filter isDigit (tokens !! 1)
        supported = map (filter (/= ',')) (drop 3 tokens)
        info      = case weight of
                      Nothing -> Nothing
                      Just w  -> Just (ProgramInfo prog w supported)

parseInt :: String -> Maybe Int
parseInt s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing

---

type BalanceMap = M.Map Program (Weight, [Program])

solve2 :: [ProgramInfo] -> Weight
solve2 programs
  = case findBottom programs of
      Nothing     -> 0
      Just bottom -> case fixImbalance programs bottom of
                       Nothing          -> 0
                       Just (_, weight) -> weight

fixImbalance :: [ProgramInfo] -> Program -> Maybe (Program, Weight)
fixImbalance programs bottom
  = case findUnbalancedBranch balmap bottom of
      Nothing             -> Nothing
      Just (branch, diff) -> Just (fixWeight balmap branch diff)
  where balmap = foldr insert M.empty programs
        insert (ProgramInfo prog weight sups) = M.insert prog (weight, sups)

fixWeight :: BalanceMap -> Program -> Weight -> (Program, Weight)
fixWeight balmap bottom diff
  = case findUnbalancedBranch balmap bottom of
      Nothing              -> (bottom, weight + diff)
      Just (branch, diff') -> fixWeight balmap branch diff'
  where (weight, _) = M.findWithDefault (0, []) bottom balmap

-- This is terrible!
findUnbalancedBranch :: BalanceMap -> Program -> Maybe (Program, Weight)
findUnbalancedBranch balmap bottom
  | length groups /= 2 || length unique /= 1  = Nothing
  | otherwise                                 = Just (branch, others - weight)
  where (_, branch)   = head $ filter ((== weight). fst) (zip weights branches)
        others        = (head . head) $ filter ((/=1) . length) groups
        weight        = head (head unique)
        unique        = filter ((==1) . length) groups
        groups        = group (sort weights)
        weights       = map (totalWeight balmap) branches
        (_, branches) = M.findWithDefault (0, []) bottom balmap

totalWeight :: BalanceMap -> Program -> Weight
totalWeight balmap prog = weight + sum (map (totalWeight balmap) supported)
  where (weight, supported) = M.findWithDefault (0, []) prog balmap

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let programs = processInput input
  putStrLn $ "Part 1: " ++ show (solve1 programs)
  putStrLn $ "Part 2: " ++ show (solve2 programs)
