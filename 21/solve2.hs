
import qualified Data.Map as M (Map, fromList, lookup)

type Bit = Int

type Image = [[Bit]]

type Rules = M.Map Image Image

bitAt :: Image -> Int -> Int -> Bit
bitAt img r c = (img !! r) !! c

enhance :: Rules -> Image -> Maybe Image
enhance rules img
  | size `mod` 2 == 0   = mergeBlocks 3 (replaceBlocks rules (splitImage img 2))
  | size `mod` 3 == 0   = mergeBlocks 4 (replaceBlocks rules (splitImage img 3))
  | otherwise           = Nothing
  where size = length img

splitImage :: Image -> Int -> [[Image]]
splitImage img blksize = [[block (r * blksize) (c * blksize) |
                          c <- [0..lastidx]] | r <- [0..lastidx]]
  where lastidx = ((length img) - 1) `div` blksize
        block dr dc = [[bitAt img (dr + r) (dc + c) |
                       c <- [0.. blksize - 1]] | r <- [0.. blksize - 1]]

replaceBlocks :: Rules -> [[Image]] -> Maybe [[Image]]
replaceBlocks rules = sequence . map (sequence . map fetch)
  where fetch b = M.lookup b rules

mergeBlocks :: Int -> Maybe [[Image]] -> Maybe Image
mergeBlocks _       Nothing       = Nothing
mergeBlocks blksize (Just blocks) = Just [[bit r c |
                                          c <- [0..imgsize - 1]] |
                                          r <- [0..imgsize - 1]]
  where imgsize = blksize * (length blocks)
        bit r c = bitAt (block r c) (r - blksize * (r `div` blksize)) (c - blksize * (c `div` blksize))
        block r c = (blocks !! (r `div` blksize)) !! (c `div` blksize)


---

processRules :: [(Image, Image)] -> Rules
processRules = M.fromList . concat . map expandRule

expandRule :: (Image, Image) -> [(Image, Image)]
expandRule (pattern, img) = [(flipf (rotate pattern r), img) |
                             r <- [0..size * size - 1],
                             flipf <- [noFlip, horizontalFlip, verticalFlip]]
  where size = length img
        noFlip img = img

rotate :: Image -> Int -> Image
rotate img r = undefined

horizontalFlip :: Image -> Image
horizontalFlip img = undefined

verticalFlip :: Image -> Image
verticalFlip img = undefined

---

sample :: Image
sample = [[0, 1, 0]
        ,[0, 0, 1]
        ,[1, 1, 1]]

rules :: Rules
rules = M.fromList [rule1, rule2]

rule1 :: (Image, Image)
rule1 = ([[0, 0]
         ,[0, 1]]
        ,[[1, 1, 0]
         ,[1, 0, 0]
         ,[0, 0, 0]])

rule2 :: (Image, Image)
rule2 = ([[0, 1, 0]
         ,[0, 0, 1]
         ,[1, 1, 1]]
        ,[[1, 0, 0, 1]
         ,[0, 0, 0, 0]
         ,[0, 0, 0, 0]
         ,[1, 0, 0, 1]])

img :: Image
img = [[0, 0, 0, 0],[0, 1, 0, 1],[0, 0, 0, 0],[0, 1, 0, 1]]
