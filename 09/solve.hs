
import Test.Hspec

solve1 :: String -> Int
solve1 = score1 0 False

score1 :: Int -> Bool -> String -> Int
score1 0     False []         = 0
score1 _     _     []         = error "unbalanced braces"
score1 level False ('<':xs)   = score1 level True xs
score1 level False ('{':xs)   = score1 (level + 1) False xs
score1 level False ('}':xs)   = level + score1 (level - 1) False xs
score1 level False ( _ :xs)   = score1 level False xs
score1 level True  ('>':xs)   = score1 level False xs
score1 level True  ('!':x:xs) = score1 level True xs
score1 level True  ( _ :xs)   = score1 level True xs

---

solve2 :: String -> Int
solve2 = score2 0 False

score2 :: Int -> Bool -> String -> Int
score2 0     False []         = 0
score2 _     _     []         = error "unbalanced braces"
score2 level False ('<':xs)   = score2 level True xs
score2 level False ('{':xs)   = score2 (level + 1) False xs
score2 level False ('}':xs)   = score2 (level - 1) False xs
score2 level False ( _ :xs)   = score2 level False xs
score2 level True  ('>':xs)   = score2 level False xs
score2 level True  ('!':x:xs) = score2 level True xs
score2 level True  ( _ :xs)   = 1 + score2 level True xs

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)

test :: IO ()
test = hspec $ do
  describe "solve1" $ do
    it "solves sample 1" $ do
      solve1 "{}" `shouldBe` 1
    it "solves sample 2" $ do
      solve1 "{{{}}}" `shouldBe` 6
    it "solves sample 3" $ do
      solve1 "{{},{}}" `shouldBe` 5
    it "solves sample 4" $ do
      solve1 "{{{},{},{{}}}}" `shouldBe` 16
    it "solves sample 5" $ do
      solve1 "{<a>,<a>,<a>,<a>}" `shouldBe` 1
    it "solves sample 6" $ do
      solve1 "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
    it "solves sample 7" $ do
      solve1 "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
    it "solves sample 8" $ do
      solve1 "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
  describe "solve2" $ do
    it "solves sample 1" $ do
      solve2 "<>" `shouldBe` 0
    it "solves sample 2" $ do
      solve2 "<random characters>" `shouldBe` 17
    it "solves sample 3" $ do
      solve2 "<<<<>" `shouldBe` 3
    it "solves sample 4" $ do
      solve2 "<{!>}>" `shouldBe` 2
    it "solves sample 5" $ do
      solve2 "<!!>" `shouldBe` 0
    it "solves sample 6" $ do
      solve2 "<!!!>>" `shouldBe` 0
    it "solves sample 7" $ do
      solve2 "<{o\"i!a,<{i<a>" `shouldBe` 10
