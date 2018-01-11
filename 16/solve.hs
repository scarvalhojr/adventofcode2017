
import Test.Hspec
import Data.Maybe (catMaybes, isJust)
import Data.List (sort)
import Data.Char (ord)

data Permutation = Permutation Char Int [Int] deriving (Eq, Show)

data PermutationMove = Spin Int | Exchange Int Int | Partner Char Char
  deriving (Eq, Show)

type Dance = [PermutationMove]

---

solve1 :: Permutation -> Dance -> String
solve1 perm = producePermutation . foldl move perm

makePermutation :: Char -> Int -> Permutation
makePermutation ch len = Permutation ch len [0..len - 1]

producePermutation :: Permutation -> [Char]
producePermutation (Permutation ch len perm) = map snd $ sort (zip perm [ch..])

move :: Permutation -> PermutationMove -> Permutation
move p (Spin s)         = spin p s
move p (Exchange p1 p2) = exchange p p1 p2
move p (Partner c1 c2)  = partner p c1 c2

spin :: Permutation -> Int -> Permutation
spin p@(Permutation base len perm) s
  | s > 0     = Permutation base len perm'
  | otherwise = p
  where perm' = map ((`mod` len) . (+s)) perm

exchange :: Permutation -> Int -> Int -> Permutation
exchange p@(Permutation base len perm) o1 o2
  | valid      = Permutation base len (map exc perm)
  | otherwise  = p
  where valid = o1 >= 0 && o1 < len && o2 >= 0 && o2 < len
        exc x
          | x == o1   = o2
          | x == o2   = o1
          | otherwise = x

partner :: Permutation -> Char -> Char -> Permutation
partner p@(Permutation base len perm) c1 c2
  | valid      = exchange p o1 o2
  | otherwise  = p
  where valid = p1 >= 0 && p1 < len && p2 >= 0 && p2 < len
        b  = ord base
        p1 = ord c1 - b
        p2 = ord c2 - b
        o1 = perm !! p1
        o2 = perm !! p2

---

solve2 :: Int -> Permutation -> Dance -> String
solve2 times start = producePermutation . repeatDance times start

repeatDance :: Int -> Permutation -> Dance -> Permutation
repeatDance times start dance = (iterateDance start dance) !! pos
  where pos = times `mod` (findCycle start dance)

findCycle :: Permutation -> Dance -> Int
findCycle start = (+1) . length . takeWhile (/= start) . tail . iterateDance start

iterateDance :: Permutation -> Dance -> [Permutation]
iterateDance start dance = iterate applyMoves start
  where applyMoves perm = foldl move perm dance

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let dance = processInput input
  let start = makePermutation 'a' 16
  putStrLn $ "Part 1: " ++ show (solve1 start dance)
  let times = 1000000000
  putStrLn $ "Part 2: " ++ show (solve2 times start dance)

processInput :: String -> Dance
processInput = catMaybes . map parseMove . words . replace ',' ' '

parseMove :: String -> Maybe PermutationMove
parseMove ('s':s)
  | isJust ms  = Just (Spin spin)
  | otherwise  = Nothing
  where ms = parseInt s
        spin = case ms of Nothing -> 0
                          Just v  -> v
parseMove ('x':xs)
  | length pos == 2  = Just (Exchange p1 p2)
  | otherwise        = Nothing
  where pos = (catMaybes . map parseInt . words . replace '/' ' ') xs
        p1 = head pos
        p2 = last pos
parseMove ('p':c1:'/':c2:[]) = Just (Partner c1 c2)
parseMove _ = Nothing

replace :: Char -> Char -> [Char] -> [Char]
replace a b = map (\c -> if c == a then b else c)

parseInt :: String -> Maybe Int
parseInt s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing

---

test :: IO ()
test = hspec $ do

  let start = makePermutation 'a' 5

  describe "spin" $ do

    it "ignores invalid input" $ do
      producePermutation (move start (Spin (-1))) `shouldBe` "abcde"
      producePermutation (move start (Spin ( 0))) `shouldBe` "abcde"
      producePermutation (move start (Spin ( 5))) `shouldBe` "abcde"

    it "correctly spins" $ do
      let perm1 = move start (Spin 3)
      producePermutation perm1 `shouldBe` "cdeab"
      let perm2 = move perm1 (Spin 1)
      producePermutation perm2 `shouldBe` "bcdea"
      let perm3 = move perm2 (Spin 4)
      producePermutation perm3 `shouldBe` "cdeab"

  describe "exchange" $ do

    it "ignores invalid input" $ do
      producePermutation (move start (Exchange 0 (-1))) `shouldBe` "abcde"
      producePermutation (move start (Exchange 0 ( 5))) `shouldBe` "abcde"
      producePermutation (move start (Exchange (-1) 0)) `shouldBe` "abcde"
      producePermutation (move start (Exchange ( 5) 0)) `shouldBe` "abcde"

    it "correctly exchanges" $ do
      let perm1 = move start (Exchange 0 3)
      producePermutation perm1 `shouldBe` "dbcae"
      let perm2 = move perm1 (Exchange 1 3)
      producePermutation perm2 `shouldBe` "dacbe"
      let perm3 = move perm2 (Exchange 2 4)
      producePermutation perm3 `shouldBe` "daebc"

  describe "parter" $ do

    it "ignores invalid input" $ do
      producePermutation (move start (Partner 'a' 'A')) `shouldBe` "abcde"
      producePermutation (move start (Partner 'a' 'f')) `shouldBe` "abcde"
      producePermutation (move start (Partner 'A' 'a')) `shouldBe` "abcde"
      producePermutation (move start (Partner 'f' 'a')) `shouldBe` "abcde"

    it "correctly partners" $ do

      let perm1 = move start (Partner 'a' 'd')
      producePermutation perm1 `shouldBe` "dbcae"
      let perm2 = move perm1 (Partner 'd' 'e')
      producePermutation perm2 `shouldBe` "ebcad"
      let perm3 = move perm2 (Partner 'c' 'a')
      producePermutation perm3 `shouldBe` "ebacd"
