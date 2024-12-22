import System.IO  
import Control.Monad
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.Split as Splt

type Point = (Int, Int)
type CoordMap = Map.Map Char [Point]

updateMap :: Char -> [(Int, Int)] -> CoordMap -> CoordMap
updateMap c p m = Map.insertWith (++) c p m

updateMapByRow :: String -> Int -> CoordMap -> CoordMap
updateMapByRow row rowIdx m = foldl (\acc colIdx -> if (row !! colIdx) == '.' 
                                                    then acc 
                                                    else updateMap (row !! colIdx) [(rowIdx, colIdx)] acc) 
                                    m 
                                    [0..(length row - 1)]

getCoordMap :: [String] -> CoordMap
getCoordMap grid = foldl (\m rowIdx -> updateMapByRow (grid !! rowIdx) rowIdx m ) Map.empty [0..(length grid - 1)]

areCoordsValid :: Int -> Int -> Point -> Bool
areCoordsValid numRows numCols (x, y) = x >= 0 && y >= 0 && x < numRows && y < numCols

getAntipodals :: Int -> Int -> Point -> Point -> [Point]
getAntipodals numRows numCols p1 p2 = filter (areCoordsValid numRows numCols) allAPs
    where allAPs = [(fst p1 - fst diff, snd p1 - snd diff), (fst p2 + fst diff, snd p2 + snd diff)]
          diff = (fst p2 - fst p1, snd p2 - snd p1)

getAllAntipodalsOfType :: Int -> Int -> [Point] -> [Point]
getAllAntipodalsOfType _ _ [] = [] 
getAllAntipodalsOfType _ _ [x] = [] 
getAllAntipodalsOfType numRows numCols (x:positions) = currAPs ++ getAllAntipodalsOfType numRows numCols positions
    where currAPs = foldl (\acc pos -> getAntipodals numRows numCols x pos ++ acc) [] positions

getAllAntipodals :: Int -> Int -> CoordMap -> [Point]
getAllAntipodals numRows numCols cMap = foldl (\acc x -> x ++ acc) [] $ map (getAllAntipodalsOfType numRows numCols) (map snd $ Map.toList cMap) 

getNumUniqueAntipodals :: [String] -> Int
getNumUniqueAntipodals grid = Set.size $ Set.fromList $ getAllAntipodals numRows numCols cMap
    where cMap = getCoordMap grid
          numRows = length grid
          numCols = length $ grid !! 0

-- Part 2

getNextAntipodal :: Point -> Point -> Point
getNextAntipodal pt dir = (fst pt + fst dir, snd pt + snd dir)

getPrevAntipodal :: Point -> Point -> Point
getPrevAntipodal pt dir = (fst pt - fst dir, snd pt - snd dir)

getNextAntipodalsRecursive :: Int -> Int -> Point -> Point -> [Point] -> [Point]
getNextAntipodalsRecursive numRows numCols p1 p2 currAPs = if areCoordsValid numRows numCols nextAntipodal 
                                                       then getNextAntipodalsRecursive numRows numCols p2 nextAntipodal (nextAntipodal:currAPs)
                                                       else currAPs
    where nextAntipodal = getNextAntipodal p2 (fst p2 - fst p1, snd p2 - snd p1)

getPrevAntipodalsRecursive :: Int -> Int -> Point -> Point -> [Point] -> [Point]
getPrevAntipodalsRecursive numRows numCols p1 p2 currAPs = if areCoordsValid numRows numCols prevAntipodal
                                                           then getPrevAntipodalsRecursive numRows numCols prevAntipodal p1 (prevAntipodal:currAPs)
                                                           else currAPs
    where prevAntipodal = getPrevAntipodal p1 (fst p2 - fst p1, snd p2 - snd p1)

getAllAntipodalsOnLine :: Int -> Int -> Point -> Point -> [Point]
getAllAntipodalsOnLine numRows numCols p1 p2 = prevAntipodals ++ [p1, p2] ++ nextAntipodals
    where prevAntipodals = getPrevAntipodalsRecursive numRows numCols p1 p2 []
          nextAntipodals = getNextAntipodalsRecursive numRows numCols p1 p2 []

getAllAntipodalsOfTypePart2 :: Int -> Int -> [Point] -> [Point]
getAllAntipodalsOfTypePart2 _ _ [] = [] 
getAllAntipodalsOfTypePart2 _ _ [x] = [] 
getAllAntipodalsOfTypePart2 numRows numCols (x:positions) = currAPs ++ getAllAntipodalsOfTypePart2 numRows numCols positions
    where currAPs = foldl (\acc pos -> getAllAntipodalsOnLine numRows numCols x pos ++ acc) [] positions

getAllAntipodalsPart2 :: Int -> Int -> CoordMap -> [Point]
getAllAntipodalsPart2 numRows numCols cMap = foldl (\acc x -> x ++ acc) [] $ map (getAllAntipodalsOfTypePart2 numRows numCols) (map snd $ Map.toList cMap)

getNumUniqueAntipodalsPart2 :: [String] -> Int
getNumUniqueAntipodalsPart2 grid = Set.size $ Set.fromList $ getAllAntipodalsPart2 numRows numCols cMap
    where cMap = getCoordMap grid
          numRows = length grid
          numCols = length $ grid !! 0

main = do  
        let list = []
        handle <- openFile "input.txt" ReadMode
        contents <- hGetContents handle
        let input = Splt.splitOn "\n" contents
        
        let simpleTest = ["..........", 
                          "..........", 
                          "..........", 
                          "....a.....", 
                          "..........", 
                          ".....a....", 
                          "..........", 
                          "..........", 
                          "..........", 
                          ".........."]

        let test = ["............", 
                    "........0...", 
                    ".....0......", 
                    ".......0....", 
                    "....0.......", 
                    "......A.....", 
                    "............", 
                    "............", 
                    "........A...", 
                    ".........A..", 
                    "............", 
                    "............"]

        let simpleMap = getCoordMap simpleTest
        let testMap = getCoordMap test

        -- Part 1
        -- print $ getNumUniqueAntipodals input

        -- Part 2
        print $ getNumUniqueAntipodalsPart2 input


        hClose handle


toInt :: [String] -> [Int]
toInt = map read