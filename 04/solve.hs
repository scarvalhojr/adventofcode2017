
import Data.List (sort, group)

type Passphrase = String

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates = any ((>1) . length) . group . sort

countValid :: (Passphrase -> Bool) -> [Passphrase] -> Int
countValid f = length . filter f

isValid1 :: Passphrase -> Bool
isValid1 = not . hasDuplicates . words

---

isAnagram :: Passphrase -> Passphrase -> Bool
isAnagram x y = (sort x) == (sort y)

isValid2 :: Passphrase -> Bool
isValid2 p = not $ (hasDuplicates tokens) || (any (uncurry isAnagram) pairs)
  where pairs  = [(x, y) | x <- tokens, y <- tokens, x /= y]
        tokens = words p

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let passphrases = lines input
  let sol1 = countValid isValid1 passphrases
  putStrLn $ "Part 1: " ++ show (sol1)
  let sol2 = countValid isValid2 passphrases
  putStrLn $ "Part 1: " ++ show (sol2)
