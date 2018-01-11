
import Data.Bits (xor, (.&.))


type Value = Integer
type Factor = Integer
type Modulus = Integer

matchLast16Bits :: Value -> Value -> Bool
matchLast16Bits x y = (x .&. mask) `xor` (y .&. mask) == 0
  where mask = 65535

countMatches :: [Value] -> [Value] -> Int -> Int
countMatches xs ys iter = length matches
  where matches = filter (==True) comps
        comps   = take (iter + 1) $ zipWith matchLast16Bits xs ys

generator :: Factor -> Modulus -> Value -> [Value]
generator factor modulus = iterate (gen factor modulus)
  where gen f m p = (p * f) `mod` m

genA :: Value -> [Value]
genA = generator 16807 2147483647

genB :: Value -> [Value]
genB = generator 48271 2147483647

iter1 :: Int
iter1 = 40000000

---

pickyGenerator :: Value -> [Value] -> [Value]
pickyGenerator m = filter ((==0) . (`mod` m))

pickyGenA :: Value -> [Value]
pickyGenA = pickyGenerator 4 . genA

pickyGenB :: Value -> [Value]
pickyGenB = pickyGenerator 8 . genB

iter2 :: Int
iter2 = 5000000

---

sample :: IO ()
sample = do
  let startA = 65
  let startB = 8921

  let sol1 = countMatches (genA startA) (genB startB) iter1
  putStrLn $ "Part 1: " ++ (show sol1)

  let sol2 = countMatches (pickyGenA startA) (pickyGenB startB) iter2
  putStrLn $ "Part 2: " ++ (show sol2)

---

main :: IO ()
main = do
  let startA = 883
  let startB = 879

  let sol1 = countMatches (genA startA) (genB startB) iter1
  putStrLn $ "Part 1: " ++ (show sol1)

  let sol2 = countMatches (pickyGenA startA) (pickyGenB startB) iter2
  putStrLn $ "Part 2: " ++ (show sol2)
