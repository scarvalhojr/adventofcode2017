
import Data.Maybe (catMaybes)
import Data.List (delete, maximumBy)

type Port = Int
type Component = (Port, Port)
type Strength = Int
data Bridge = Bridge Strength [Component]
  deriving (Eq, Show)

instance Ord Bridge where
  compare (Bridge x _) (Bridge y _) = compare x y

longest :: Bridge -> Bridge -> Ordering
longest (Bridge s1 comp1) (Bridge s2 comp2)
  | lencomp == EQ   = compare s1 s2
  | otherwise       = lencomp
  where lencomp = compare (length comp1) (length comp2)

buildBridge :: [Component] -> Port -> Bridge
buildBridge components port
  | null possible   = Bridge 0 []
  | otherwise       = strongest
  where possible = filter (canConnect port) components
        options  = map (\c -> addComponent c (buildBridge (delete c components) (otherPort port c))) possible
        strongest = maximum options

buildBridge2 :: [Component] -> Port -> Bridge
buildBridge2 components port
  | null possible   = Bridge 0 []
  | otherwise       = strongest
  where possible = filter (canConnect port) components
        options  = map (\c -> addComponent c (buildBridge2 (delete c components) (otherPort port c))) possible
        strongest = maximumBy longest options

-- TODO: Return Maybe Port
otherPort :: Port -> Component -> Port
otherPort port (p1, p2)
  | port == p1  = p2
  | otherwise   = p1

canConnect :: Port -> Component -> Bool
canConnect port (p1, p2) = port == p1 || port == p2

addComponent :: Component -> Bridge -> Bridge
addComponent c@(p1, p2) (Bridge strength comps) = Bridge strength' (c: comps)
  where strength' = strength + p1 + p2

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let components = processInput input
  let start = 0
  -- let (Bridge strength1 _) = buildBridge components 0
  -- putStrLn $ "Part 1: " ++ show (strength1)
  let (Bridge strength2 _) = buildBridge2 components 0
  putStrLn $ "Part 2: " ++ show (strength2)

processInput :: String -> [Component]
processInput = catMaybes . map parseComponent . lines

parseComponent :: String -> Maybe Component
parseComponent input
  | length ports == 2   = Just (c1, c2)
  | otherwise           = Nothing
  where parse = catMaybes . map parseInt . words . replace '/' ' '
        ports = parse input
        c1   = head ports
        c2   = last ports

replace :: Char -> Char -> [Char] -> [Char]
replace a b = map (\c -> if c == a then b else c)

parseInt :: String -> Maybe Int
parseInt s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing
