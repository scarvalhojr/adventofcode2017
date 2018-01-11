
type Blocks = Int
type Length = Int
data Memory = Memory Length [Blocks]
  deriving (Eq, Show)

createMemory :: [Blocks] -> Memory
createMemory blocks = Memory (length blocks) blocks

reallocate :: Memory -> Memory
reallocate (Memory len blocks) = Memory len blocks'
  where maxblk  = maximum blocks
        (b, a)  = splitBy maxblk blocks
        blocks' = before ++ (newbank : after)
        before  = b
        after   = a
        newbank = maxblk `div` len

splitBy :: Eq a => a -> [a] -> ([a], [a])
splitBy _ []     = ([], [])
splitBy v (x:xs)
  | x == v    = ([], xs)
  | otherwise = (x : fst s, snd s)
  where s = splitBy v xs
