
import Data.Maybe (catMaybes)
import Data.List (sort, groupBy)

type ParticleID = Int
data Position = Position Int Int Int
  deriving (Eq, Show)
data Velocity = Velocity Int Int Int
  deriving (Eq, Show)
data Acceleration = Acceleration Int Int Int
  deriving (Eq, Show)
data Particle = Particle ParticleID Position Velocity Acceleration
  deriving (Eq, Show)

instance Ord Position where
  compare (Position x1 y1 z1) (Position x2 y2 z2)
    | d1 /= d2   = compare d1 d2
    | x1 /= x2   = compare x1 x2
    | y1 /= y2   = compare y1 y2
    | otherwise  = compare z1 z2
    where d1 = abs x1 + abs y1 + abs z1
          d2 = abs x2 + abs y2 + abs z2

instance Ord Acceleration where
  compare (Acceleration x1 y1 z1) (Acceleration x2 y2 z2) = compare a1 a2
    where a1 = abs x1 + abs y1 + abs z1
          a2 = abs x2 + abs y2 + abs z2

instance Ord Particle where
  compare (Particle _ pos1 _ _) (Particle _ pos2 _ _) = compare pos1 pos2

minAcceleration :: [Particle] -> ParticleID
minAcceleration = snd . minimum . map getPartAcc
  where getPartAcc (Particle pid _ _ acc) = (acc, pid)

---

converge :: [Particle] -> [Particle]
converge []     = []
converge (p:[]) = [p]
converge ps     = updateAndConverge ps' (length ps') (minPairDistance ps')
  where ps' = removeCollisions ps

updateAndConverge :: [Particle] -> Int -> Int -> [Particle]
updateAndConverge [] _ _     = []
updateAndConverge (p:[]) _ _ = [p]
updateAndConverge ps len mindist
  | len' < 2               = ps'
  | len' < len
    || changing
    || mindist' < mindist  = updateAndConverge ps' len' mindist'
  | otherwise              = ps
  where ps'       = updateAll ps
        len'      = length ps'
        mindist'  = minPairDistance ps'
        changing = or (map changingDirection ps')

removeCollisions :: [Particle] -> [Particle]
removeCollisions = concat . filter ((==1) . length). groupBy collide . sort

collide :: Particle -> Particle -> Bool
collide (Particle _ pos1 _ _) (Particle _ pos2 _ _) = pos1 == pos2

updateAll :: [Particle] -> [Particle]
updateAll = removeCollisions . map updateParticle

updateParticle :: Particle -> Particle
updateParticle (Particle pid pos vel acc) = Particle pid pos' vel' acc
  where pos' = move pos vel'
        vel' = accelerate vel acc

move :: Position -> Velocity -> Position
move (Position x y z) (Velocity vx vy vz) = Position x' y' z'
  where x' = x + vx
        y' = y + vy
        z' = z + vz

accelerate :: Velocity -> Acceleration -> Velocity
accelerate (Velocity x y z) (Acceleration ax ay az) = Velocity x' y' z'
  where x' = x + ax
        y' = y + ay
        z' = z + az

changingDirection :: Particle -> Bool
changingDirection (Particle _ _ (Velocity vx vy vz) (Acceleration ax ay az))
  = changing vx ax || changing vy ay || changing vz az
  where changing _ 0 = False
        changing 0 _ = True
        changing v a = (v > 0) /= (a > 0)

minPairDistance :: [Particle] -> Int
minPairDistance []         = error "empty list"
minPairDistance (_:[])     = error "list must contain at least 2 elements"
minPairDistance (p1:p2:[]) = particleDistance p1 p2
minPairDistance (p:ps)     = min (minDistanceFrom p ps) (minPairDistance ps)

minDistanceFrom :: Particle -> [Particle] -> Int
minDistanceFrom _ [] = error "empty list"
minDistanceFrom p ps = minimum (map (particleDistance p) ps)

particleDistance :: Particle -> Particle -> Int
particleDistance (Particle _ pos1 _ _) (Particle _ pos2 _ _)
  = positionDistance pos1 pos2

positionDistance :: Position -> Position -> Int
positionDistance (Position x1 y1 z1) (Position x2 y2 z2) = dx + dy + dz
  where dx = abs (x1 - x2)
        dy = abs (y1 - y2)
        dz = abs (z1 - z2)

---

main :: IO ()
main = do
  let inputFile = "input.txt"
  putStrLn $ "Reading input from: " ++ inputFile
  input <- readFile inputFile
  let particles = processInput input
  putStrLn $ "Part 1: " ++ show (minAcceleration particles)
  putStrLn $ "Part 2: " ++ show (length (converge particles))

processInput :: String -> [Particle]
processInput = catMaybes . map parseLine . zip [0..] . lines
  where parseLine (id, line) = parseParticle id line

parseParticle :: ParticleID -> String -> Maybe Particle
parseParticle pid s
  | length tokens == 3  = makeParticle pid pos vel acc
  | otherwise           = Nothing
  where tokens = map parseXYZ (words s)
        pos = makePosition (tokens !! 0)
        vel = makeVelocity (tokens !! 1)
        acc = makeAcceleration (tokens !! 2)

makePosition :: [Int] -> Maybe Position
makePosition (x:y:z:[]) = Just (Position x y z)
makePosition _ = Nothing

makeVelocity :: [Int] -> Maybe Velocity
makeVelocity (x:y:z:[]) = Just (Velocity x y z)
makeVelocity _ = Nothing

makeAcceleration :: [Int] -> Maybe Acceleration
makeAcceleration (x:y:z:[]) = Just (Acceleration x y z)
makeAcceleration _ = Nothing

makeParticle :: ParticleID
             -> Maybe Position
             -> Maybe Velocity
             -> Maybe Acceleration
             -> Maybe Particle
makeParticle _ Nothing _ _ = Nothing
makeParticle _ _ Nothing _ = Nothing
makeParticle _ _ _ Nothing = Nothing
makeParticle pid (Just pos) (Just vel) (Just acc)
  = Just (Particle pid pos vel acc)

parseXYZ :: String -> [Int]
parseXYZ = catMaybes . map parseInt . words . replace ',' ' ' . removeMarkers
  where removeMarkers = filter (not . (`elem` ['p', 'v', 'a', '=', '<', '>']))

replace :: Char -> Char -> [Char] -> [Char]
replace a b = map (\c -> if c == a then b else c)

parseInt :: String -> Maybe Int
parseInt s = case reads s of
               [(val, "")] -> Just val
               _           -> Nothing
