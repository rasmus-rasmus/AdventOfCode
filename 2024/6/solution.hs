import System.IO  
import Control.Monad
import Data.List
import qualified Data.List.Split as Splt
import qualified Data.Set as Set
import Debug.Trace

data CardinalDirection = North | East | South | West deriving (Show, Enum, Bounded, Eq, Ord)

toDir :: CardinalDirection -> (Int, Int)
toDir card = case card of
                North -> (-1, 0)
                East -> (0, 1)
                South -> (1, 0)
                West -> (0, -1)

rotateCounterClockwise :: CardinalDirection -> CardinalDirection
rotateCounterClockwise card = case card of
                                North -> East
                                East -> South
                                South -> West
                                West -> North

data Guard = Guard { position :: (Int, Int), direction :: CardinalDirection} deriving (Show, Eq, Ord)

rotateGuard :: Guard -> Guard
rotateGuard guardIn = Guard {position=(position guardIn), direction=(rotateCounterClockwise (direction guardIn))}

takeStep :: Guard -> Guard
takeStep guardIn = Guard {position=newPos, direction=(direction guardIn)}
    where newPos = (fst oldPos + fst dir, snd oldPos + snd dir)
          oldPos = position guardIn
          dir = toDir . direction $ guardIn

getIndexOfGuardRecursive :: String -> Int
getIndexOfGuardRecursive "" = 0
getIndexOfGuardRecursive (x:rest) = if x == '^' then 0 else 1 + getIndexOfGuardRecursive rest

getIndexOfGuard :: String -> Int
getIndexOfGuard row = if res >= (length row) then -1 else res
    where res = getIndexOfGuardRecursive row

getGuard :: [String] -> Guard
getGuard map = Guard {position=(rowIdx, colIdx), direction=North}
    where rowIdx = head (filter (\row -> getIndexOfGuard (map !! row) >= 0) [0.. (length map - 1)])
          colIdx = getIndexOfGuard (map !! rowIdx)

-- '!' means out of bounds
getNextSymbol :: [String] -> Guard -> Char
getNextSymbol map guard = if i < 0 || i >= length map || j < 0 || j >= length (map !! i)
                          then '!'
                          else (map !! i) !! j
    where (i, j) = position . takeStep $ guard

walk :: [String] -> Guard -> [Guard] -> [Guard]
walk map guard visited = case nextSymbol of
                            '!' -> (guard):visited
                            '#' -> walk map (rotateGuard guard) visited
                            '.' -> walk map (takeStep guard) ((guard):visited)
                            '^' -> walk map (takeStep guard) ((guard):visited)
                            x -> error "unexpected symbol"
    where nextSymbol = getNextSymbol map guard

getNumDistinctVisited :: [String] -> Int
getNumDistinctVisited map = Set.size $ Set.fromList visited
    where visited = walk map (getGuard map) []


-- Part 2

getUniqueVisited :: [String] -> [(Int, Int)]
getUniqueVisited theMap = Set.toList . Set.fromList $ map position visited
    where visited = walk theMap (getGuard theMap) []

isWalkPeriodic :: [String] -> Guard -> Set.Set Guard -> (Int, Int) -> Bool
isWalkPeriodic map guard visited extraObstacle
    | guard `Set.member` visited = True
    | nextSymbol == '!' = False
    | nextPos == extraObstacle || nextSymbol == '#' = isWalkPeriodic map (rotateGuard guard) (Set.insert guard visited) extraObstacle
    | nextSymbol == '.' || nextSymbol == '^' = isWalkPeriodic map (takeStep guard) (Set.insert guard visited) extraObstacle
        where nextPos = position . takeStep $ guard
              nextSymbol = getNextSymbol map guard

isWalkPeriodicDebugWrapper :: [String] -> Guard -> Set.Set Guard -> (Int, Int) -> Bool
isWalkPeriodicDebugWrapper map guard visited extraObstacle = trace ("Checking: " ++ (show extraObstacle)) $ isWalkPeriodic map guard visited extraObstacle

getNumPeriodicObstacles :: [String] -> Int
getNumPeriodicObstacles theMap = length $ filter (isWalkPeriodic theMap guard (Set.fromList [])) $ filter (/= position guard) (getUniqueVisited theMap)
    where guard = getGuard theMap


main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let map = Splt.splitOn "\n" contents

        let testMap = Splt.splitOn "\n" "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#..."

        let simpleTestMap = [".#.", "#^#", "..."]

        print $ getNumDistinctVisited map

        -- Part 2

        -- Runs for some minutes - not the most efficient implementation...
        print $ getNumPeriodicObstacles map
        
        hClose handle


toInt :: [String] -> [Int]
toInt = map read