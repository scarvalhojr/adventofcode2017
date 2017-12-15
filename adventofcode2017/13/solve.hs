
import Test.Hspec
import Control.Monad (msum)
import Data.Maybe (catMaybes)

type Depth = Int
type Range = Int
data Scanner = Scanner Depth Range deriving Show
type Firewall = [Scanner]
type Severity = Int
type Time = Int

solve1 :: Firewall -> Severity
solve1 = tripSeverity 0

tripSeverity :: Time -> Firewall -> Severity
tripSeverity start = sum . catMaybes . map (severityFromStart start)

severityFromStart :: Time -> Scanner -> Maybe Severity
severityFromStart start (Scanner depth range)
  | range <= 0                           = Nothing
  | range == 1 || time `mod` cycle == 0  = Just (depth * range)
  | otherwise                            = Nothing
  where time = start + depth
        cycle = 2 * (range - 1)

processInput :: String -> Firewall
processInput = catMaybes . map parseInputLine . lines

parseInputLine :: String -> Maybe Scanner
parseInputLine line
  | length tokens == 2  = Just (Scanner depth range)
  | otherwise           = Nothing
  where tokens = (catMaybes . map parseInt . words . filter (/=':')) line
        depth = head tokens
        range = last tokens

parseInt :: String -> Maybe Int
parseInt s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing

---

solve2 :: Firewall -> Time
solve2 firewall = head $ filter (notCaught firewall) [0..]

notCaught :: Firewall -> Time -> Bool
notCaught firewall start = (null . msum) $ map (severityFromStart start) firewall

---

test :: IO ()
test = hspec $ do

  let sample = "0: 3\n1: 2\n4: 4\n6: 4"
  let firewall = processInput sample

  describe "solve1" $ do
    it "solves sample input" $ do
      solve1 firewall `shouldBe` 24

  describe "solve2" $ do
    it "solves sample input" $ do
      solve2 firewall `shouldBe` 10

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let firewall = processInput input
  putStrLn $ "Part 1: " ++ show (solve1 firewall)
  putStrLn $ "Part 2: " ++ show (solve2 firewall)
