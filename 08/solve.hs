
import Test.Hspec
import qualified Data.Map as M (Map, empty, insertWith, lookup, fromList)

type RegisterTable = M.Map Register Value

type Register = String

type Value = Integer

data Instruction = Instruction Register Operation Value Condition deriving Show

data Operation = Inc
               | Dec
               deriving (Eq, Show)

data Condition = Condition Register Comparison Value deriving Show

data Comparison = LessThan
                | LessThanOrEqual
                | Equal
                | NotEqual
                | GreaterThanOrEqual
                | GreaterThan
                deriving Show

parseInstruction :: String -> Maybe Instruction
parseInstruction inst
  | length tokens /= 7  = Nothing
  | otherwise           = buildInstruction reg oper val condreg comp condval
  where [reg, oper_t, val_t, _, condreg, comp_t, condval_t] = tokens
        tokens  = words inst
        oper    = parseOperation oper_t
        val     = parseValue val_t
        comp    = parseComparison comp_t
        condval = parseValue condval_t

buildInstruction :: Register
                 -> Maybe Operation
                 -> Maybe Value
                 -> Register
                 -> Maybe Comparison
                 -> Maybe Value
                 -> Maybe Instruction
buildInstruction _ Nothing _ _ _ _ = Nothing
buildInstruction _ _ Nothing _ _ _ = Nothing
buildInstruction _ _ _ _ Nothing _ = Nothing
buildInstruction _ _ _ _ _ Nothing = Nothing
buildInstruction reg (Just op) (Just val) condreg (Just comp) (Just condval)
  = Just (Instruction reg op val (Condition condreg comp condval))

parseOperation :: String -> Maybe Operation
parseOperation "inc" = Just Inc
parseOperation "dec" = Just Dec
parseOperation _     = Nothing

parseComparison :: String -> Maybe Comparison
parseComparison "<"  = Just LessThan
parseComparison "<=" = Just LessThanOrEqual
parseComparison "==" = Just Equal
parseComparison "!=" = Just NotEqual
parseComparison ">=" = Just GreaterThanOrEqual
parseComparison ">"  = Just GreaterThan
parseComparison _    = Nothing

parseValue :: String -> Maybe Value
parseValue s = case reads s of
                 [(val, "")] -> Just val
                 _           -> Nothing

--

solve1 :: String -> Integer
solve1 s
  | null table  = 0
  | otherwise   = maximum table
  where table = processInput s

processInput :: String -> RegisterTable
processInput = foldl processInstruction M.empty . map parseInstruction . lines

processInstruction :: RegisterTable -> Maybe Instruction -> RegisterTable
processInstruction table Nothing = table
processInstruction table (Just (Instruction reg oper val cond))
  | isValid   = M.insertWith (+) reg opValue table
  | otherwise = table
  where isValid = isValidCondition table cond
        opValue = case oper of Inc -> val
                               Dec -> negate val

isValidCondition :: RegisterTable -> Condition -> Bool
isValidCondition table (Condition reg comp val) =
  case comp of LessThan           -> stored <  val
               LessThanOrEqual    -> stored <= val
               Equal              -> stored == val
               NotEqual           -> stored /= val
               GreaterThanOrEqual -> stored >= val
               GreaterThan        -> stored >  val
  where stored = case M.lookup reg table of Nothing -> 0
                                            Just s  -> s

--

solve2 :: String -> Integer
solve2 = snd . processInputTrackMax

processInputTrackMax :: String -> (RegisterTable, Value)
processInputTrackMax s = foldl processInstructionTrackMax (M.empty, 0) inst
  where inst = map parseInstruction (lines s)

processInstructionTrackMax :: (RegisterTable, Value)
                           -> Maybe Instruction
                           -> (RegisterTable, Value)
processInstructionTrackMax (table, maxval) inst = (table', maxval')
  where table'  = processInstruction table inst
        maxval' = if table' == M.empty then
                    maxval
                  else
                    max maxval (maximum table')

--

test :: IO ()
test = hspec $ do

  let sample = "b inc   5 if a >   1\n\
               \a inc   1 if b <   5\n\
               \c dec -10 if a >=  1\n\
               \c inc -20 if c == 10"

  describe "solve1" $ do
    it "solves sample input" $ do
      solve1 sample `shouldBe` 1
      processInput sample `shouldBe` M.fromList [("a", 1), ("c", -10)]

  describe "solve2" $ do
    it "solves sample input 1" $ do
      let i = "b inc 5 if a > 1\n\
              \a inc 1 if b < 5\n\
              \c dec -10 if a >= 1\n\
              \c inc -20 if c == 10"
      solve2 sample `shouldBe` 10
      processInputTrackMax sample `shouldBe`
        (M.fromList [("a", 1), ("c", -10)], 10)

--

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  putStrLn $ "Part 1: " ++ show (solve1 input)
  putStrLn $ "Part 2: " ++ show (solve2 input)
