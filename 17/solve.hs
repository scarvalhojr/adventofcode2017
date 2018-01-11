
import Test.Hspec

type Repetitions = Int
type Steps = Int
type Position = Int
type Value = Int
data Buffer = Buffer Position [Value] deriving (Eq, Show)

solve1 :: Repetitions -> Steps -> Value
solve1 rep = afterLast . spinlock (Buffer 0 [0]) rep

spinlock :: Buffer -> Repetitions -> Steps -> Buffer
spinlock start rep steps = (iterate (insert steps) start) !! rep

insert :: Steps -> Buffer -> Buffer
insert steps (Buffer pos values) = Buffer pos' values'
  where pos'    = 1 + ((pos + steps) `mod` (length values))
        newval  = 1 + values !! pos
        (a, b)  = splitAt pos' values
        values' = a ++ newval : b

afterLast :: Buffer -> Value
afterLast (Buffer pos values) = values !! pos'
  where pos' = (pos + 1) `mod` (length values)

---

type Length = Int
data SingleBuffer = SingleBuffer Position Value Length deriving (Eq, Show)

solve2 :: Repetitions -> Steps -> Value
solve2 rep steps = val
  where (SingleBuffer _ val _) = singleSpinlock (SingleBuffer 0 0 1) rep steps

singleSpinlock :: SingleBuffer -> Repetitions -> Steps -> SingleBuffer
singleSpinlock buffer 0   _     = buffer
singleSpinlock buffer rep steps = singleSpinlock buffer' (rep - 1) steps
  where buffer' = singleInsert steps buffer

singleInsert :: Steps -> SingleBuffer -> SingleBuffer
singleInsert steps (SingleBuffer pos val len) = SingleBuffer pos' val' (len + 1)
  where pos' = 1 + ((pos + steps) `mod` len)
        val' = if (pos' == 1) then len else val

---

main :: IO ()
main = do
  let reps1 = 2017
  let steps = 369
  putStrLn $ "Part 1: " ++ show (solve1 reps1 steps)
  let reps2 = 50000000
  putStrLn $ "Part 2: " ++ show (solve2 reps2 steps)

---

test :: IO ()
test = hspec $ do

  describe "insert" $ do
    it "correctly inserts" $ do
      let startBuffer = Buffer 0 [0]
      let result = take 10 $ iterate (insert 3) startBuffer
      result `shouldBe` [Buffer 0 [0],
                         Buffer 1 [0,1],
                         Buffer 1 [0,2,1],
                         Buffer 2 [0,2,3,1],
                         Buffer 2 [0,2,4,3,1],
                         Buffer 1 [0,5,2,4,3,1],
                         Buffer 5 [0,5,2,4,3,6,1],
                         Buffer 2 [0,5,7,2,4,3,6,1],
                         Buffer 6 [0,5,7,2,4,3,8,6,1],
                         Buffer 1 [0,9,5,7,2,4,3,8,6,1]]

  describe "solve1" $ do
    it "solves sample input" $ do
      solve1 2017 3 `shouldBe` 638

  describe "singleInsert" $ do
    it "correctly inserts" $ do
      let startBuffer = SingleBuffer 0 0 1
      let result = take 10 $ iterate (singleInsert 3) startBuffer
      result `shouldBe` [SingleBuffer 0 0 1,
                         SingleBuffer 1 1 2,
                         SingleBuffer 1 2 3,
                         SingleBuffer 2 2 4,
                         SingleBuffer 2 2 5,
                         SingleBuffer 1 5 6,
                         SingleBuffer 5 5 7,
                         SingleBuffer 2 5 8,
                         SingleBuffer 6 5 9,
                         SingleBuffer 1 9 10]
