
-- 37  36  35  34  33  32  31
-- 38  17  16  15  14  13  30
-- 39  18   5   4   3  12  29
-- 40  19   6   1   2  11  28
-- 41  20   7   8   9  10  27
-- 42  21  22  23  24  25  26
-- 43  44  45  46  47  48  49

data SpiralPos = SpiralPos Int Int deriving (Eq, Show)

instance Enum SpiralPos where
  succ (SpiralPos x y)
    | x >  y && x >  -y    = SpiralPos (  x  ) (y - 1)
    | x >  y && x <= -y    = SpiralPos (x - 1) (  y  )
    | x <= y && x <  -y    = SpiralPos (  x  ) (y + 1)
    | otherwise            = SpiralPos (x + 1) (  y  )

  pred (SpiralPos x y)
    | x >  0 && x == y + 1 = SpiralPos (x - 1) (  y  )
    | x >  y && x >= -y    = SpiralPos (  x  ) (y + 1)
    | x >= y && x <  -y    = SpiralPos (x + 1) (  y  )
    | x < y  && x <= -y    = SpiralPos (  x  ) (y - 1)
    | otherwise            = SpiralPos (x - 1) (  y  )

  fromEnum (SpiralPos 0 0) = 0
  fromEnum s               = 1 + fromEnum (pred s)

  toEnum 0                 = SpiralPos 0 0
  toEnum x                 = succ (toEnum (x - 1))

instance Ord SpiralPos where
  compare s s' = compare (fromEnum s) (fromEnum s')

manhattanDistance :: SpiralPos -> Int
manhattanDistance (SpiralPos x y) = abs x + abs y

solve1 :: Int -> Int
solve1 x = manhattanDistance (last (take x spiralEnum))
  where spiralEnum = enumFrom (toEnum 0)

-- Part Two

-- 147 142 133 122  59
-- 304   5   4   2  57
-- 330  10   1   1  54
-- 351  11  23  25  26
-- 362 747 806 880 931

predAdjacent :: SpiralPos -> [SpiralPos]
predAdjacent s = filter (< s) (adjacent s)
  where adjacent (SpiralPos x y) = [SpiralPos (x + dx) (y + dy) |
                                    dx <- [-1, 0, 1], dy <- [-1, 0, 1],
                                    dx /= 0 || dy /= 0]

-- Slow but more readable
accValue' :: SpiralPos -> Int
accValue' (SpiralPos 0 0) = 1
accValue' s = (sum . map accValue' . predAdjacent) s

-- Faster version with memoization (https://wiki.haskell.org/Memoization)
accValue :: SpiralPos -> Int
accValue = (map acc [SpiralPos 0 0..] !!) . fromEnum
  where acc (SpiralPos 0 0) = 1
        acc s               = (sum . map accValue . predAdjacent) s

solve2 :: Int -> Int
solve2 x = head $ dropWhile (<= x) (map accValue [SpiralPos 0 0..])

-- Main

input :: Int
input = 361527

main :: IO ()
main = do putStrLn $ "Part 1: " ++ show (solve1 input)
          putStrLn $ "Part 2: " ++ show (solve2 input)
