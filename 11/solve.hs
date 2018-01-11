
import Test.Hspec

data HexMove = North | NorthEast | SouthEast | South | SouthWest | NorthWest
  deriving (Eq, Show)

data HexPos = HexPos Int Int Int
  deriving (Eq, Show)

solve1 :: String -> Int
solve1 = distanceToOrigin . reduceHexPos . foldr hexMove hexOrigin . parseInput
  where parseInput  = map parseMove . words . replace ',' ' '
        replace a b = map (\c -> if c == a then b else c)

hexOrigin :: HexPos
hexOrigin = HexPos 0 0 0

hexMove :: Maybe HexMove -> HexPos -> HexPos
hexMove Nothing          pos              = pos
hexMove (Just North    ) (HexPos n ne nw) = HexPos (n + 1) ne      nw
hexMove (Just South    ) (HexPos n ne nw) = HexPos (n - 1) ne      nw
hexMove (Just NorthEast) (HexPos n ne nw) = HexPos  n     (ne + 1) nw
hexMove (Just SouthWest) (HexPos n ne nw) = HexPos  n     (ne - 1) nw
hexMove (Just NorthWest) (HexPos n ne nw) = HexPos  n      ne     (nw + 1)
hexMove (Just SouthEast) (HexPos n ne nw) = HexPos  n      ne     (nw - 1)

parseMove :: String -> Maybe HexMove
parseMove "n"  = Just North
parseMove "ne" = Just NorthEast
parseMove "se" = Just SouthEast
parseMove "s"  = Just South
parseMove "sw" = Just SouthWest
parseMove "nw" = Just NorthWest
parseMove _    = Nothing

distanceToOrigin :: HexPos -> Int
distanceToOrigin (HexPos n ne nw) = abs n + abs ne + abs nw

reduceHexPos :: HexPos -> HexPos
reduceHexPos pos@(HexPos n ne nw)
  | n  > 0 && ne < 0   = reduceHexPos $ HexPos (n - 1) (ne + 1) (nw + 1)
  | n  > 0 && nw < 0   = reduceHexPos $ HexPos (n - 1) (ne + 1) (nw + 1)
  | n  < 0 && ne > 0   = reduceHexPos $ HexPos (n + 1) (ne - 1) (nw + 1)
  | n  < 0 && nw > 0   = reduceHexPos $ HexPos (n + 1) (ne + 1) (nw - 1)
  | ne > 0 && nw > 0   = reduceHexPos $ HexPos (n + 1) (ne - 1) (nw - 1)
  | ne < 0 && nw < 0   = reduceHexPos $ HexPos (n - 1) (ne + 1) (nw + 1)
  | otherwise          = pos

---

solve2 :: String -> Int
solve2 = snd . foldl hexMoveTrackMax (hexOrigin, 0) . parseInput
  where parseInput  = map parseMove . words . replace ',' ' '
        replace a b = map (\c -> if c == a then b else c)

hexMoveTrackMax :: (HexPos, Int) -> Maybe HexMove -> (HexPos, Int)
hexMoveTrackMax (pos, maxdist) move = (pos', maxdist')
  where pos'     = hexMove move pos
        maxdist' = max maxdist (distanceToOrigin (reduceHexPos pos'))

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 1: " ++ show (solve2 input)

---

test :: IO ()
test = hspec $ do

  describe "solve1" $ do
    it "solves sample input 1" $ do
      solve1 "ne,ne,ne" `shouldBe` 3
    it "solves sample input 2" $ do
      solve1 "ne,ne,sw,sw" `shouldBe` 0
    it "solves sample input 3" $ do
      solve1 "ne,ne,s,s" `shouldBe` 2
    it "solves sample input 4" $ do
      solve1 "se,sw,se,sw,sw" `shouldBe` 3
