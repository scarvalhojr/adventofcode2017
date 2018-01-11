
import qualified Data.Map as M (Map, fromList, lookup)

type Bit = Int

type Image = [[Bit]]

type Rules = M.Map Image Image

bitAt :: Image -> Int -> Int -> Bit
bitAt img r c = (img !! r) !! c

enhance :: Image -> Maybe Image
enhance = undefined

splitImage :: Image -> Int -> [[Image]]
splitImage img blksize = [[block (r * blksize) (c * blksize) |
                          c <- [0..lastidx]] | r <- [0..lastidx]]
  where lastidx = ((length img) - 1) `div` blksize
        block dr dc = [[bitAt img (dr + r) (dc + c) |
                       c <- [0.. blksize - 1]] | r <- [0.. blksize - 1]]

replaceBlocks :: Rules -> [[Image]] -> Maybe [[Image]]
replaceBlocks rules = sequence . map (sequence . map fetch)
  where fetch b = M.lookup b rules

mergeBlocks :: Maybe [[Image]] -> Int -> Maybe Image
mergeBlocks Nothing _ = Nothing
mergeBlocks (Just blocks) blksize = Just [[bit r c |
                                          c <- [0..imgsize - 1]] |
                                          r <- [0..imgsize - 1]]
  where imgsize = blksize * (length blocks)
        bit r c = bitAt (block r c) (r - blksize * ((r `div` blksize) - 1)) (c - blksize * ((c `div` blksize) - 1))
        block r c = (blocks !! (r `div` blksize)) !! (c `div` blksize)

---

---

start :: Image
start = [[0, 1, 0]
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
