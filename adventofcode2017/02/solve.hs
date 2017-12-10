
import Data.Maybe (fromMaybe)

parseSpreadsheet :: String -> [[Int]]
parseSpreadsheet = map (map read) . map words . lines

solve1 :: String -> Int
solve1 = sum . map diff . parseSpreadsheet
  where diff row = maximum row - minimum row

findWholeDivision :: [Int] -> Maybe Int
findWholeDivision []     = Nothing
findWholeDivision (x:xs) =
  case findDiv x xs
    of Nothing -> findWholeDivision xs
       Just q  -> Just q
  where findDiv x [] = Nothing
        findDiv x (y:ys)
          | rem x y == 0  = Just (quot x y)
          | rem y x == 0  = Just (quot y x)
          | otherwise     = findDiv x ys

solve2 :: String -> Int
solve2 = fromMaybe 0 . fmap sum . sequence . wholeDivisions
  where wholeDivisions = map findWholeDivision . parseSpreadsheet

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)

sample1 :: String
sample1 = "5 1 9 5\n\
          \7 5 3  \n\
          \2 4 6 8"

sample2 :: String
sample2 = "5 9 2 8\n\
          \9 4 7 3\n\
          \3 8 6 5"
