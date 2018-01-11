
import Data.Char (isDigit, digitToInt, isSpace)

safeDigitToInt :: Char -> Int
safeDigitToInt d
  | isDigit d  = digitToInt d
  | otherwise  = 0

solve1 :: [Char] -> Int
solve1 []     = 0
solve1 (x:xs) = sumRepeated ((x:xs) ++ [x])
  where sumRepeated []       = 0
        sumRepeated [_]      = 0
        sumRepeated (x':x:xs)
          | x' == x   = (safeDigitToInt x) + sumRepeated (x:xs)
          | otherwise = sumRepeated (x:xs)

solve2 :: [Char] -> Int
solve2 [] = 0
solve2 xs
  | len `mod` 2 == 0  = 2 * sumRepeated (splitAt half xs)
  | otherwise         = 0
  where len  = length xs
        half = len `div` 2
        sumRepeated ([]    , _     ) = 0
        sumRepeated (_     , []    ) = 0
        sumRepeated ((x:xs), (y:ys))
          | x == y    = (safeDigitToInt x) + sumRepeated (xs, ys)
          | otherwise = sumRepeated (xs, ys)

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  rawInput <- readFile inputFile
  let input = filter (not . isSpace) rawInput
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
