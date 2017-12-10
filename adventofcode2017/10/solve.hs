
import Data.Char (isAscii, ord)
import Data.Bits (xor)
import Numeric (showHex)
import Test.Hspec

type HashLength = Int
type SkipLength = Int
type HeadPosition = Int

data KnotHash = KnotHash [Int] HashLength HeadPosition SkipLength
  deriving (Eq, Show)

solve1 :: Int -> [Int] -> Int
solve1 len = hash . foldl twistKnotHash (makeKnotHash len)

hash :: KnotHash -> Int
hash (KnotHash list _ _ _) = foldr (*) 1 (take 2 list)

makeKnotHash :: HashLength -> KnotHash
makeKnotHash x = KnotHash (take x [0..]) x 0 0

twistKnotHash :: KnotHash -> Int -> KnotHash
twistKnotHash (KnotHash xs len head skip) twist = KnotHash xs' len head' skip'
  where (h, t) = splitAt twist (rotate head xs)
        xs'    = rotate (-head) (reverse h ++ t)
        skip'  = (skip + 1) `mod` len
        head'  = (head + twist + skip) `mod` len

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs
  | n == 0    = xs
  | n <  0    = rotate (n + length xs) xs
  | otherwise = zipWith const (drop n (cycle xs)) xs

---

solve2 :: Int -> String -> String
solve2 len = hashString . denseHash . twist . convertInput
  where twist  = foldl twistKnotHash (makeKnotHash len)

hashString :: [Int] -> String
hashString [] = ""
hashString (x:xs)
  | x < 0 || x > 255  = hashString xs
  | x < 16            = '0' : (showHex x "") ++ hashString xs
  | otherwise         = (showHex x "") ++ hashString xs

denseHash :: KnotHash -> [Int]
denseHash (KnotHash list _ _ _) = xor16 list
  where xor16 [] = []
        xor16 xs = (foldr xor 0 (take 16 xs)) : xor16 (drop 16 xs)

convertInput :: String -> [Int]
convertInput = concat . take 64 . repeat . convert
  where convert [] = [17, 31, 73, 47, 23]
        convert (x:xs)
          | isAscii x   = ord x : convert xs
          | otherwise   = convert xs

---

test :: IO ()
test = hspec $ do

  let sampleHashSize = 5
  let sampleInput = [3, 4, 1, 5]

  describe "solve1" $ do
    it "solves sample input" $ do
      solve1 sampleHashSize sampleInput `shouldBe` 12

  describe "twistKnotHash" $ do
    it "solves sample input" $ do
      scanl twistKnotHash (makeKnotHash sampleHashSize) sampleInput
        `shouldBe`[KnotHash [0,1,2,3,4] sampleHashSize 0 0,
                   KnotHash [2,1,0,3,4] sampleHashSize 3 1,
                   KnotHash [4,3,0,1,2] sampleHashSize 3 2,
                   KnotHash [4,3,0,1,2] sampleHashSize 1 3,
                   KnotHash [3,4,2,1,0] sampleHashSize 4 4]

  describe "solve2" $ do
    it "solves sample inputs" $ do
      solve2 hashSize ""         `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      solve2 hashSize "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      solve2 hashSize "1,2,3"    `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      solve2 hashSize "1,2,4"    `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"

---

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (solve1 hashSize input)
  putStrLn $ "Part 2: " ++ show (solve2 hashSize inputString)

hashSize :: Int
hashSize = 256

input :: [Int]
input = [106, 118, 236, 1, 130, 0, 235, 254, 59, 205, 2, 87, 129, 25, 255, 118]

inputString :: String
inputString = "106,118,236,1,130,0,235,254,59,205,2,87,129,25,255,118"
