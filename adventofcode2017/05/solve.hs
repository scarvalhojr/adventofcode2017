
type Offset = Int
type Length = Int
type Position = Int
type Count = Int
data State = State [Offset] Length Position Count
  deriving Show

createState :: [Offset] -> State
createState offsets = State offsets (length offsets) 0 0

execute1 :: State -> State
execute1 s@(State offsets len pos count)
  | pos < 0 || pos >= len   = s
  | otherwise               = execute1 (State offsets' len pos' (count + 1))
  where offsets' = before ++ ((jump + 1) : after)
        before   = take pos offsets
        after    = drop (pos + 1) offsets
        pos'     = pos + jump
        jump     = offsets !! pos

---

execute2 :: State -> State
execute2 s@(State offsets len pos count)
  | pos < 0 || pos >= len   = s
  | otherwise               = execute2 (State offsets' len pos' (count + 1))
  where offsets' = before ++ (jump' : after)
        before   = take pos offsets
        after    = drop (pos + 1) offsets
        pos'     = pos + jump
        jump     = offsets !! pos
        jump'    = jump + if jump >= 3 then -1 else 1

---

sample :: IO ()
sample = do
  let inputFile = "sample.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let offsets = map parseInt (lines input)
  let start = createState offsets
  putStrLn $ "Part 1: " ++ show (execute1 start)
  putStrLn $ "Part 2: " ++ show (execute2 start)

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let offsets = map parseInt (lines input)
  let start = createState offsets
  let (State _ _ _ count1) = execute1 start
  putStrLn $ "Part 1: " ++ show count1
  let (State _ _ _ count2) = execute2 start
  putStrLn $ "Part 2: " ++ show count2

parseInt :: String -> Int
parseInt s = case reads s of
               [(val, "")] -> val
               _           -> error ("Invalid input: " ++ s)
